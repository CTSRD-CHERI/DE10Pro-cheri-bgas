/*-
 * Copyright (c) 2021-2022 Alexandre Joannou
 * All rights reserved.
 *
 * This material is based upon work supported by the DoD Information Analysis
 * Center Program Management Office (DoD IAC PMO), sponsored by the Defense
 * Technical Information Center (DTIC) under Contract No. FA807518D0004.  Any
 * opinions, findings and conclusions or recommendations expressed in this
 * material are those of the author(s) and do not necessarily reflect the views
 * of the Air Force Installation Contracting Agency (AFICA).
 *
 * @BERI_LICENSE_HEADER_START@
 *
 * Licensed to BERI Open Systems C.I.C. (BERI) under one or more contributor
 * license agreements.  See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.  BERI licenses this
 * file to you under the BERI Hardware-Software License, Version 1.0 (the
 * "License"); you may not use this file except in compliance with the
 * License.  You may obtain a copy of the License at:
 *
 *   http://www.beri-open-systems.org/legal/license-1-0.txt
 *
 * Unless required by applicable law or agreed to in writing, Work distributed
 * under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
 * CONDITIONS OF ANY KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations under the License.
 *
 * @BERI_LICENSE_HEADER_END@
 */

  Recipe r = rSeq ( rBlock (
      //recipeDelay (450000)
      recipeDelay (1000)
    , debugUnitWriteReg (verbosity, 7'h10, 'h80000001)
    , debugUnitReadReg (verbosity, 7'h11)
    //reg 7'h11 = 'h382
    , debugUnitWriteReg (verbosity, 7'h17, 'h3207b0)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h400000d3
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h4, 'h4000b0d3)
    , debugUnitWriteReg (verbosity, 7'h5, 'h0)
    , debugUnitWriteReg (verbosity, 7'h17, 'h3307b0)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h11)
    //reg 7'h11 = 'h382
    , debugUnitWriteReg (verbosity, 7'h17, 'h3207b0)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h4000b0d3
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    /*
    , debugUnitWriteReg (verbosity, 7'h17, 'h321000)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h321001)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h321002)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h321003)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h321004)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h321005)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h321006)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h321007)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h321008)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h321009)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h32100a)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h32100b)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h32100c)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h32100d)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h32100e)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h32100f)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h321010)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h321011)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h321012)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h321013)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h321014)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h321015)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h321016)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h321017)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h321018)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h321019)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h32101a)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h32101b)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h32101c)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h32101d)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h32101e)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h32101f)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    */
    , debugUnitWriteReg (verbosity, 7'h17, 'h3207b1)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h70000000
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20040807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h70000000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h6ffffffc)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    /*
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h0)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h0)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h4)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h4)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h8)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h8)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'hc)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'hc)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h10)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h10)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h14)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h14)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h18)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h18)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h1c)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h1c)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h20)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h20)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h24)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h24)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h28)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h28)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h2c)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h2c)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h30)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h30)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h34)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h34)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h38)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h38)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h3c)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h3c)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h40)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h40)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h44)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h44)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h48)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h48)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h4c)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h4c)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h50)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h50)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h54)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h54)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h58)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h58)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h5c)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h5c)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h60)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'h60)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'h0
    , debugUnitWriteReg (verbosity, 7'h4, 'hc0000000)
    , debugUnitWriteReg (verbosity, 7'h5, 'h0)
    , debugUnitWriteReg (verbosity, 7'h17, 'h3307b1)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    */
    /*
    , debugUnitWriteReg (verbosity, 7'h17, 'h321000)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h321001)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h321002)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h321003)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h321004)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h321005)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h321006)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h321007)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h321008)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h321009)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h32100a)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h32100b)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h32100c)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h32100d)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h32100e)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h32100f)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h321010)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h321011)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h321012)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h321013)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h321014)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h321015)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h321016)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h321017)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h321018)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h321019)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h32101a)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h32101b)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h32101c)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h32101d)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h32101e)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitWriteReg (verbosity, 7'h17, 'h32101f)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'h0
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    */
    , debugUnitWriteReg (verbosity, 7'h17, 'h3207b1)
    , debugUnitReadReg (verbosity, 7'h16)
    //reg 7'h16 = 'h2
    , debugUnitReadReg (verbosity, 7'h4)
    //reg 7'h4 = 'hc0000000
    , debugUnitReadReg (verbosity, 7'h5)
    //reg 7'h5 = 'h0
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h2015b807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'hc0000000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitReadReg (verbosity, 7'h3c)
    //reg 7'h3c = 'hffffffff
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h38, 'h2055f000)
    , debugUnitReadReg (verbosity, 7'h38)
    //reg 7'h38 = 'h20158807
    , debugUnitWriteReg (verbosity, 7'h3a, 'h0)
    , debugUnitWriteReg (verbosity, 7'h39, 'hbffffffc)
    , debugUnitReadReg (verbosity, 7'h38)
    , done.send
    ));

