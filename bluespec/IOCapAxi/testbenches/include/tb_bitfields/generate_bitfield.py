"""
This python program generates code in various langauges (TODO just C++ for now) to pack and unpack structures from primitive types
"""

from collections import defaultdict
import dataclasses
from typing import Dict, List, Optional, Tuple, Union

@dataclasses.dataclass(frozen=True)
class Field:
    name: str
    width: int

    def __post_init__(self):
        if self.width < 1:
            raise RuntimeError(f"Cannot generate field '{self.field}' with <1 size")

@dataclasses.dataclass(frozen=True)
class BackingPrimitive:
    width: int


@dataclasses.dataclass(frozen=True)
class BackingArray:
    of: BackingPrimitive
    len: int
    msb: bool

    @staticmethod
    def MSB(of: BackingPrimitive, len: int) -> "BackingArray":
        return BackingArray(of, len, msb=True)
    
    @staticmethod
    def LSB(of: BackingPrimitive, len: int) -> "BackingArray":
        return BackingArray(of, len, msb=False)


U8 = BackingPrimitive(8)
U16 = BackingPrimitive(16)
U32 = BackingPrimitive(32)
U64 = BackingPrimitive(64)


Backing = Union[BackingPrimitive, BackingArray]

# A structure is an ordered list of fields.
# Fields with empty names are used to specify padding in the backing
@dataclasses.dataclass(frozen=True)
class Struct:
    name: str
    backing: Backing
    fields: List[Field]

class Generator:
    def generate(self, struct: Struct):
        ...

@dataclasses.dataclass(frozen=True)
class OrComponent:
    extract_name: str
    extract_type: BackingPrimitive
    extract_first_bit: int
    extract_width: int
    insert_position: int

class CppGenerator:
    bit_types = {
        x: f"uint{x}_t"
        for x in (8, 16, 32, 64)
    }
    literal_suffix = {
        32: "u",
        64: "ul",
    }

    def __init__(self):
        # self.namespace = namespace
        pass

    def cpp_type_for_backing(self, backing: Backing) -> str:
        if isinstance(backing, BackingPrimitive):
            if backing.width in self.bit_types:
                return self.bit_types[backing.width]
            else:
                raise RuntimeError(f"C++ cannot express the backing type {backing}. Must be of widths {[str(x) for x in self.bit_types.keys()]}")
        else:
            # isinstance(backing, BackingArray)
            return f"std::array<{self.cpp_type_for_backing(backing.of)}, {backing.len}>"
            # return f"{self.cpp_type_for_backing(backing.of)}[{backing.len}]"
        
    def literal_of_backing_type(self, value: int, base: int, backing: BackingPrimitive) -> str:
        if base == 16:
            literal = f"0x{value:x}"
        elif base == 10:
            literal = f"{value:d}"
        else:
            raise RuntimeError(f"C++ cannot express literals in base {base}")
        
        # TODO check it fits

        if backing.width in self.literal_suffix:
            return literal + self.literal_suffix[backing.width]
        else:
            # Take a 64-bit literal and cast it down
            return f"{self.cpp_type_for_backing(backing)}({literal}{self.literal_suffix[64]})"
        
    def layout_backing_plan(self, name: str, backing: Backing) -> List[Tuple[str, BackingPrimitive]]:
        if isinstance(backing, BackingPrimitive):
            return [(name, backing)]
        else:
            # isinstance(backing, BackingArray)
            if backing.msb:
                # index [0] = most significant
                # => put it last in the list
                return sum(
                    [
                        self.layout_backing_plan(f"{name}[{idx}]", backing.of)
                        for idx in reversed(range(backing.len))
                    ]
                    , start=[]
                )
            else:
                return sum(
                    [
                        self.layout_backing_plan(f"{name}[{idx}]", backing.of)
                        for idx in range(backing.len)
                    ]
                    , start=[]
                )

    def backing_prim_for_field(self, field: Field) -> BackingPrimitive:
        if field.width <= 8:
            return U8
        elif field.width <= 16:
            return U16
        elif field.width <= 32:
            return U32
        elif field.width <= 64:
            return U64
        else:
            raise RuntimeError(f"C++ cannot express the field {field}. Maximum size 64 bits")
        
    def cpp_type_for_field(self, field: Field) -> str:
        return self.cpp_type_for_backing(self.backing_prim_for_field(field))
    
    # OR bit-fields from multiple names together to create a single primitive
    def generate_or_expression(self, output_name: str, output_backing: BackingPrimitive, or_components: List[OrComponent]) -> str:
        or_strategies = []
        for or_component in or_components:
            # Shift the component down so extract_first_bit is at 0, unless we are extracting directly from bit 0
            if (or_component.extract_first_bit == 0) and False:
                comp = f"({or_component.extract_name})"
            else:
                comp = f"({or_component.extract_name} >> {self.literal_of_backing_type(or_component.extract_first_bit, base=10, backing=U32)})"
            # Mask out the N bits we want.
            # If we're going to cast the value *down* to a type with exactly the width we expect, that has the same effect as the mask and we don't need it.
            # If we're going to cast the value *up*, and the current width is exactly the width we expect, we don't need to mask.
            if False and (or_component.extract_type.width != output_backing.width) and (or_component.extract_width == min(or_component.extract_type.width, output_backing.width)):
                comp = comp
            else:
                mask = (1 << or_component.extract_width) - 1
                comp = f"({comp} & {self.literal_of_backing_type(mask, base=16, backing=or_component.extract_type)})"
            # Cast the value to the expected type
            if (or_component.extract_type.width != output_backing.width):
                comp = f"{self.cpp_type_for_backing(output_backing)}{comp}"
            # Shift the outcome back up to insert_position, unless that position is 0 and this is the only component
            if len(or_components) > 1 or or_component.insert_position > 0:
                comp = f"({comp} << {or_component.insert_position})"
            or_strategies.append(comp)

        return f"\t{output_name} = (\n\t\t" + " | \n\t\t".join(or_strategies) + "\n\t);\n"

    def generate(self, struct: Struct):
        # First, generate the struct definition
        struct_code = f"struct {struct.name} {{\n\t" \
            + "\n\t".join([
                f"/** {field.width}-bit field */\n\t{self.cpp_type_for_field(field)} {field.name};"
                for field in struct.fields
                if field.name
            ]) + "\n\n"

        backing_cpp_name = "backing"
        backing_cpp_type = self.cpp_type_for_backing(struct.backing)
        backing_cpp_plan = self.layout_backing_plan(backing_cpp_name, struct.backing)

        # Then, generate an unpack member function AND a pack member function
        unpack_func = f"\tstatic {struct.name} unpack(const {backing_cpp_type}& {backing_cpp_name}) {{\n" \
            + f"\t{struct.name} value{{}};\n"
        pack_func = f"\t{backing_cpp_type} pack() const {{\n" \
            + f"\t{backing_cpp_type} {backing_cpp_name}{{}};\n"

        pack_plan: Dict[Tuple[str, BackingPrimitive], List[OrComponent]] = defaultdict(list)
        idx_backing_elem = 0
        idx_backing_elem_bit = 0
        for field in struct.fields:
            bits_needed = field.width
            or_components = []
            while bits_needed > 0:
                (backing_elem_name, backing_elem_prim) = backing_cpp_plan[idx_backing_elem]
                if (bits_needed + idx_backing_elem_bit) < backing_elem_prim.width:
                    # We don't need all of this field.
                    # Consume only the bits we need.
                    or_components.append(OrComponent(
                        backing_elem_name, backing_elem_prim,
                        extract_first_bit=idx_backing_elem_bit,
                        extract_width=bits_needed,
                        insert_position=(field.width - bits_needed)
                    ))
                    # This backing element will be packed with the requisite parts of this field
                    if field.name:
                        pack_plan[(backing_elem_name, backing_elem_prim)].append(OrComponent(
                            field.name,
                            self.backing_prim_for_field(field),
                            extract_first_bit=(field.width - bits_needed),
                            extract_width=bits_needed,
                            insert_position=idx_backing_elem_bit,
                        ))
                    idx_backing_elem_bit += bits_needed
                    bits_needed = 0
                else:
                    # (bits_needed + idx_backing_elem_bit) >= backing_elem_prim.width
                    # We need the rest of this field.
                    or_components.append(OrComponent(
                        backing_elem_name, backing_elem_prim,
                        extract_first_bit=idx_backing_elem_bit,
                        extract_width=(backing_elem_prim.width - idx_backing_elem_bit),
                        insert_position=(field.width - bits_needed)
                    ))
                    # This backing element will be packed with the requisite parts of this field
                    if field.name:
                        pack_plan[(backing_elem_name, backing_elem_prim)].append(OrComponent(
                            field.name,
                            self.backing_prim_for_field(field),
                            extract_first_bit=(field.width - bits_needed),
                            extract_width=(backing_elem_prim.width - idx_backing_elem_bit),
                            insert_position=idx_backing_elem_bit,
                        ))
                    bits_needed -= (backing_elem_prim.width - idx_backing_elem_bit)
                    idx_backing_elem += 1
                    idx_backing_elem_bit = 0
                pass
            if field.name:
                unpack_func += self.generate_or_expression(
                    f"value.{field.name}",
                    self.backing_prim_for_field(field),
                    or_components
                )
        pass

        unpack_func += "\treturn value;\n}"
        struct_code += unpack_func.replace("\n", "\n\t") + "\n"


        # Then, generate a pack member function
        for ((backing_elem_name, backing_elem_prim), or_components) in pack_plan.items():
            pack_func += self.generate_or_expression(
                backing_elem_name, # already has {backing_cpp_name} at the front
                backing_elem_prim,
                or_components
            )

        pack_func += f"\treturn {backing_cpp_name};\n}}\n"
        struct_code += pack_func.replace("\n", "\n\t")

        struct_code = struct_code + "\n};\n"
        return struct_code


BLUESPEC_IOCAPAXI_STRUCTS = [
    Struct(
        "AWFlit_id4_addr64_user3",
        BackingArray.LSB(U32, 4),
        [
            Field("awid", 4),
            Field("awaddr", 64),
            Field("awlen", 8),
            Field("awsize", 3),
            Field("awburst", 2),
            Field("awlock", 1),
            Field("awcache", 4),
            Field("awprot", 3),
            Field("awqos", 4),
            Field("awregion", 8),
            Field("awuser", 3),
        ],
    ),
    Struct(
        "WFlit_data32",
        U64,
        [
            Field("wdata", 32),
            Field("wstrb", 4),
            Field("wlast", 1),
        ],
    ),
    Struct(
        "BFlit_id4",
        U8,
        [
            Field("bid", 4),
            Field("bresp", 2),
        ],
    ),
    Struct(
        "ARFlit_id4_addr64_user3",
        BackingArray.LSB(U32, 4),
        [
            Field("arid", 4),
            Field("araddr", 64),
            Field("arlen", 8),
            Field("arsize", 3),
            Field("arburst", 2),
            Field("arlock", 1),
            Field("arcache", 4),
            Field("arprot", 3),
            Field("arqos", 4),
            Field("arregion", 8),
            Field("aruser", 3),
        ],
    ),
    Struct(
        "RFlit_id4_data32",
        U64,
        [
            Field("rid", 4),
            Field("rdata", 32),
            Field("rresp", 2),
            Field("rlast", 1),
        ],
    ),
]

BLUESPEC_SANITIZEDAXI_STRUCTS = [
    Struct(
        "AWFlit_id4_addr64_user0",
        BackingArray.LSB(U32, 4),
        [
            Field("awid", 4),
            Field("awaddr", 64),
            Field("awlen", 8),
            Field("awsize", 3),
            Field("awburst", 2),
            Field("awlock", 1),
            Field("awcache", 4),
            Field("awprot", 3),
            Field("awqos", 4),
            Field("awregion", 8),
            Field("awuser", 3),
        ],
    ),
    Struct(
        "WFlit_data32",
        U64,
        [
            Field("wdata", 32),
            Field("wstrb", 4),
            Field("wlast", 1),
        ],
    ),
    Struct(
        "BFlit_id4",
        U8,
        [
            Field("bid", 4),
            Field("bresp", 2),
        ],
    ),
    Struct(
        "ARFlit_id4_addr64_user0",
        BackingArray.LSB(U32, 4),
        [
            Field("arid", 4),
            Field("araddr", 64),
            Field("arlen", 8),
            Field("arsize", 3),
            Field("arburst", 2),
            Field("arlock", 1),
            Field("arcache", 4),
            Field("arprot", 3),
            Field("arqos", 4),
            Field("arregion", 8),
        ],
    ),
    Struct(
        "RFlit_id4_data32",
        U64,
        [
            Field("rid", 4),
            Field("rdata", 32),
            Field("rresp", 2),
            Field("rlast", 1),
        ],
    ),
]

BLUESPEC_TUPLE2_KEYID_MAYBE_KEY = [
    Struct(
        "Tuple2_KeyId_MaybeKey",
        BackingArray.LSB(U32, 5),
        [
            Field("keyId", 8),
            Field("keyValid", 1),
            Field("keyTop", 64),
            Field("keyBot", 64),
        ]
    )
]

STRUCTS = BLUESPEC_SANITIZEDAXI_STRUCTS

print("// Autogenerated by generate_bitfield.py")
print("// Requires <cstdint> and <array>, does not include them by default because this file may be included inside of a namespace")
print("")
for struct in STRUCTS:
    print(CppGenerator().generate(struct))
