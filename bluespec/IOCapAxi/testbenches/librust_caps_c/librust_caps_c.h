#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

enum CCapPerms
#ifdef __cplusplus
  : uint8_t
#endif // __cplusplus
 {
  Read = 1,
  Write = 2,
  ReadWrite = 3,
};
#ifndef __cplusplus
typedef uint8_t CCapPerms;
#endif // __cplusplus

enum CCapResult
#ifdef __cplusplus
  : int32_t
#endif // __cplusplus
 {
  Success = 0,
  Encode_UnrepresentableBaseRange = 1,
  Encode_UnrepresentableCaveat = 2,
  Encode_InvalidCaveat = 3,
  Encode_NoCaveatsLeft = 4,
  Encode_CantShrinkPerms = 5,
  Decode_InvalidCaveat = 6,
  Decode_InvalidSignature = 7,
  Decode_InvalidCapPermsChain = 8,
  NullRequiredArgs = 100,
};
#ifndef __cplusplus
typedef int32_t CCapResult;
#endif // __cplusplus

/**
 * Little-endian representation of a 128-bit number
 */
typedef uint8_t CCapU128[16];

typedef struct CCap2024_02 {
  CCapU128 signature;
  CCapU128 data;
} CCap2024_02;

#ifdef __cplusplus
extern "C" {
#endif // __cplusplus

/**
 * Initialize a Cap2024_02 capability from scratch allowing access to the full 64-bit address range (base = 0, len = 1<<64), given the permissions (Read|Write|Both), and the secret id.
 * Calculates the capability signature given the packed data and the secret.
 *
 * cap and secret are non-optional, and the function returns `NullRequiredArgs` if either are null.
 *
 * Does not use caveats.
 */
CCapResult ccap_init_almighty(struct CCap2024_02 *cap,
                              const CCapU128 *secret,
                              uint32_t secret_id,
                              CCapPerms perms);

/**
 * Initialize a Cap2024_02 capability from scratch, given the contiguous memory range it grants access to, the permissions (Read|Write|Both), and the secret_id.
 * Calculates the capability signature given the packed data and the secret.
 *
 * cap and secret are non-optional, and the function returns `NullRequiredArgs` if either are null.
 *
 * Returns an Encode error if the base/length is not exactly representable.
 * Use [ccap_init_inexact] to allow rounding the bounds up in this case instead of returning an error.
 *
 * Does not use caveats.
 */
CCapResult ccap_init_exact(struct CCap2024_02 *cap,
                           const CCapU128 *secret,
                           uint64_t base,
                           uint64_t len,
                           uint32_t secret_id,
                           CCapPerms perms);

/**
 * Initialize a Cap2024_02 capability from scratch, given the contiguous memory range it grants access to, the permissions (Read|Write|Both), and the secret_id.
 * Calculates the capability signature given the packed data and the secret.
 *
 * cap and secret are non-optional, and the function returns `NullRequiredArgs` if either are null.
 *
 * Will round the bounds up to the smallest possible value that encloses [base, base+len].
 * If exact bounds are required use [ccap_init_exact].
 *
 * Does not use caveats.
 */
CCapResult ccap_init_inexact(struct CCap2024_02 *cap,
                             const CCapU128 *secret,
                             uint64_t base,
                             uint64_t len,
                             uint32_t secret_id,
                             CCapPerms perms);

/**
 * Given a pointer to a capability, read off its base and length.
 * len_64 will be set if the range.len() has the 64th bit set.
 * base, len, and len_64 are optional arguments, and are ignored if null.
 * cap is non-optional, and the function returns `NullRequiredArgs` if null.
 * Returns a Decode error if the capability data is invalid.
 * Doesn't check the capability signature.
 */
CCapResult ccap_read_range(const struct CCap2024_02 *cap,
                           uint64_t *base,
                           uint64_t *len,
                           bool *len_64);

/**
 * Given a pointer to a capability, read off it's permissions (Read, Write, or both).
 * cap and perms are non-optional, and the function returns `NullRequiredArgs` if they're null.
 * Returns a Decode error if the capability data is invalid.
 * Doesn't check the capability signature.
 */
CCapResult ccap_read_perms(const struct CCap2024_02 *cap, CCapPerms *perms);

/**
 * Given a pointer to a capability, read off the secret-key id it claims to use.
 * cap and secret_id are non-optional, and the function returns `NullRequiredArgs` if they're null.
 * Returns a Decode error if the capability data is invalid.
 * Doesn't check the capability signature.
 */
CCapResult ccap_read_secret_id(const struct CCap2024_02 *cap, uint32_t *secret_id);

#ifdef __cplusplus
} // extern "C"
#endif // __cplusplus
