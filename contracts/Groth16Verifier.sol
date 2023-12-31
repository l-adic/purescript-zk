// SPDX-License-Identifier: GPL-3.0
/*
    Copyright 2021 0KIMS association.

    This file is generated with [snarkJS](https://github.com/iden3/snarkjs).

    snarkJS is a free software: you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    snarkJS is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
    or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
    License for more details.

    You should have received a copy of the GNU General Public License
    along with snarkJS. If not, see <https://www.gnu.org/licenses/>.
*/

pragma solidity >=0.7.0 <0.9.0;

contract Groth16Verifier {
    // Scalar field size
    uint256 constant r = 21888242871839275222246405745257275088548364400416034343698204186575808495617;
    // Base field size
    uint256 constant q = 21888242871839275222246405745257275088696311157297823662689037894645226208583;

    // Verification Key data
    uint256 constant alphax  = 0x25b43a978f454a54aee59edabd9ad9b7248ef9010d929c12ed147df909fd6618;
    uint256 constant alphay  = 0x177afa4f0d0cf2edcf4a6e6c55d92c2e7fd1f042ffe7e4563af4082b65b612c8;
    uint256 constant betax1  = 0x0967250b479cac731e4fbb3a83e44c3cc01fec317b43dc7da9a30f0abd80c3ea;
    uint256 constant betax2  = 0x27288818e63a3ab25f807bbeca86115b3dfa213bc0327c4f0a701d5250450b46;
    uint256 constant betay1  = 0x0e83f8c0ee6ba6c34f8ea99bb485a56ef16a0bac9c131ee755a1ea25d7a6d45e;
    uint256 constant betay2  = 0x2ed906cbb6a398010a1693a12712a2a80e05838c35198feaa4c6dcb0b959ff92;
    uint256 constant gammax1 = 0x2da6e58853c4300e1cbb14c43273bd3e256dd654a4a5d13356580ed817421f00;
    uint256 constant gammax2 = 0x19cc0f4f8a459e0553d610c95a1a31efa6bf05c4a946d38063f8a0c7b4f6ff2e;
    uint256 constant gammay1 = 0x1bd4bb7184bd9212d6459fd088cc668953fca2130edee820dda9756360e04242;
    uint256 constant gammay2 = 0x0f9e56cb6df87b430715221daa5bfa568437b05a420d9f0013122c66141ec495;
    uint256 constant deltax1 = 0x1edc8a6711f72da56154d14c6370e278c59a2b754299e183c28a1c7bf9e6acba;
    uint256 constant deltax2 = 0x07f661357b3e5d6c12ea467f67f15293681bbd24871febf36d2affc7cd278a09;
    uint256 constant deltay1 = 0x0eedc264e8cb4071734f9c6cbea31a74811be87f88a70ed1971920cf5c926cee;
    uint256 constant deltay2 = 0x21ad95f71ff3e65a9b2dba5c68f31f57fe984af01922d2328939ac4770de3717;

    
    uint256 constant IC0x = 0x21a6ea664bb098a3f57fb7eeee797428ab90f3c82342bf34b9edd280895616b9;
    uint256 constant IC0y = 0x0668f911f958be6c26dde0524f0e9ed92638ab145f33e1b8a437d2e021097a7f;
    
    uint256 constant IC1x = 0x16150a7330342ddc4b4ad852d8f2ca75ed2d5278e0d6b2902ca46d861e35d60a;
    uint256 constant IC1y = 0x2e7c17156493c0f0406100bd6fe7a9992fce1456fd951987a4895383ca389930;
    

    // Memory data
    uint16 constant pVk = 0;
    uint16 constant pPairing = 128;

    uint16 constant pLastMem = 896;

    function verifyProof(uint[2] calldata _pA, uint[2][2] calldata _pB, uint[2] calldata _pC, uint[1] calldata _pubSignals) public view returns (bool) {
        assembly {
            function checkField(v) {
                if iszero(lt(v, q)) {
                    mstore(0, 0)
                    return(0, 0x20)
                }
            }
            
            // G1 function to multiply a G1 value(x,y) to value in an address
            function g1_mulAccC(pR, x, y, s) {
                let success
                let mIn := mload(0x40)
                mstore(mIn, x)
                mstore(add(mIn, 32), y)
                mstore(add(mIn, 64), s)

                success := staticcall(sub(gas(), 2000), 7, mIn, 96, mIn, 64)

                if iszero(success) {
                    mstore(0, 0)
                    return(0, 0x20)
                }

                mstore(add(mIn, 64), mload(pR))
                mstore(add(mIn, 96), mload(add(pR, 32)))

                // Elliptic curve addition
                success := staticcall(sub(gas(), 2000), 6, mIn, 128, pR, 64)

                if iszero(success) {
                    mstore(0, 0)
                    return(0, 0x20)
                }
            }

            function checkPairing(pA, pB, pC, pubSignals, pMem) -> isOk {
                let _pPairing := add(pMem, pPairing)
                let _pVk := add(pMem, pVk)

                mstore(_pVk, IC0x)
                mstore(add(_pVk, 32), IC0y)

                // Compute the linear combination vk_x
                
                g1_mulAccC(_pVk, IC1x, IC1y, calldataload(add(pubSignals, 0)))
                

                // -A
                mstore(_pPairing, calldataload(pA))
                mstore(add(_pPairing, 32), mod(sub(q, calldataload(add(pA, 32))), q))

                // B
                mstore(add(_pPairing, 64), calldataload(pB))
                mstore(add(_pPairing, 96), calldataload(add(pB, 32)))
                mstore(add(_pPairing, 128), calldataload(add(pB, 64)))
                mstore(add(_pPairing, 160), calldataload(add(pB, 96)))

                // alpha1
                mstore(add(_pPairing, 192), alphax)
                mstore(add(_pPairing, 224), alphay)

                // beta2
                mstore(add(_pPairing, 256), betax1)
                mstore(add(_pPairing, 288), betax2)
                mstore(add(_pPairing, 320), betay1)
                mstore(add(_pPairing, 352), betay2)

                // vk_x
                mstore(add(_pPairing, 384), mload(add(pMem, pVk)))
                mstore(add(_pPairing, 416), mload(add(pMem, add(pVk, 32))))


                // gamma2
                mstore(add(_pPairing, 448), gammax1)
                mstore(add(_pPairing, 480), gammax2)
                mstore(add(_pPairing, 512), gammay1)
                mstore(add(_pPairing, 544), gammay2)

                // C
                mstore(add(_pPairing, 576), calldataload(pC))
                mstore(add(_pPairing, 608), calldataload(add(pC, 32)))

                // delta2
                mstore(add(_pPairing, 640), deltax1)
                mstore(add(_pPairing, 672), deltax2)
                mstore(add(_pPairing, 704), deltay1)
                mstore(add(_pPairing, 736), deltay2)

                let success := staticcall(sub(gas(), 2000), 8, _pPairing, 768, _pPairing, 0x20)

                isOk := and(success, mload(_pPairing))
            }

            let pMem := mload(0x40)
            mstore(0x40, add(pMem, pLastMem))

            // Validate that all evaluations ∈ F
            
            checkField(calldataload(add(_pubSignals, 0)))
            
            checkField(calldataload(add(_pubSignals, 32)))
            

            // Validate all evaluations
            let isValid := checkPairing(_pA, _pB, _pC, _pubSignals, pMem)

            mstore(0, isValid)
            return(0, 0x20)
        }
    }
}