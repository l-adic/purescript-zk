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

contract Verifier {
    // Scalar field size
    uint256 constant r = 21888242871839275222246405745257275088548364400416034343698204186575808495617;
    // Base field size
    uint256 constant q = 21888242871839275222246405745257275088696311157297823662689037894645226208583;

    // Verification Key data
    uint256 constant alphax  = 0x2e1e2817904007f1a5a2d081c24344f726b790ee8d9aa35c2e8d7da58fd17898;
    uint256 constant alphay  = 0x1754ac21b80ac3af4d86140b29d2eaf6049eb5368028f5679d890d3e543fbf72;
    uint256 constant betax1  = 0x1a28866e06b202369744a36d14b15f1d9983bf6eedeac9c19593ea9b34b1e4ef;
    uint256 constant betax2  = 0x1f6f75a1c5511b52f4914404abfac68b2dc50ba3397f4792a48a239b31738bfa;
    uint256 constant betay1  = 0x0a0df3826fc1f3738fdeb69de1b74fb187ea2469f36bb1f454c775f83e088af7;
    uint256 constant betay2  = 0x02070ee035b61692a96d3c5073dbe6f91733c31dfca27c29f1ba33f22776ff40;
    uint256 constant gammax1 = 0x205fc0bb8976b4304db02fe71022da4859cbe81b5ae15a1979fa27e570b3f3a9;
    uint256 constant gammax2 = 0x1809b2efbde259eb9289653085fead60873c968d383c2981343a0c38cbb71b29;
    uint256 constant gammay1 = 0x0eae180fdd84baa240b1b31f1b0fe2055bafdd50da911eb253bab8b45aa2c118;
    uint256 constant gammay2 = 0x19f1059196c0b41a68167ecb0ee33db2d8260ce30dfc791b0aad3c34a8500fcd;
    uint256 constant deltax1 = 0x1dc098821e66f27f24f617199026f8c5aff77208547b2503c13d2af2ee20852c;
    uint256 constant deltax2 = 0x0f9d8ab5d33de2f6ce77bfec0f72db3f7f6faefe4e5c2f7b3bdc9b929493400e;
    uint256 constant deltay1 = 0x276025b81092de745283fdd3cc673293988bb72695783b4b860ef0474f902b4e;
    uint256 constant deltay2 = 0x0a2b307565e9d23c8751172d27ecfacd7ae0a089150a37eadce863ef69e35f52;

    
    uint256 constant IC0x = 0x0451cc81b2ebb4969bdf68fc66967c2da31b77fbba7d0e518e46cb6a8fa07d39;
    uint256 constant IC0y = 0x2c7e6f002f75bb09ddad604b33ce776eaba1695301f0f0d9f3832a5e7033ef5e;
    
    uint256 constant IC1x = 0x0132d9f9ae38c05b4fc2fa9988bcfcb7cf72e65873790f76dbff6210927f60c3;
    uint256 constant IC1y = 0x1fccca11daaeadca85badec73da25a88e30fe76e17c329c2eafa391b5bbbe377;
    

    // Memory data
    uint16 constant pVk = 0;
    uint16 constant pPairing = 128;

    uint16 constant pLastMem = 896;

    function verifyProof(uint[2] calldata _pA, uint[2][2] calldata _pB, uint[2] calldata _pC, uint[1] calldata _pubSignals) public view returns (uint) {
        assembly {
            function checkField(v) {
                if iszero(lt(v, q)) {
                    mstore(0, 2)
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
                    mstore(0, 3)
                    return(0, 0x20)
                }

                mstore(add(mIn, 64), mload(pR))
                mstore(add(mIn, 96), mload(add(pR, 32)))

                // Elliptic curve addition
                success := staticcall(sub(gas(), 2000), 6, mIn, 128, pR, 64)

                if iszero(success) {
                    mstore(0, 4)
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

                if iszero(success) {
                    mstore(0, 5)
                    return(0, 0x20)
                }

                isOk := mload(_pPairing)
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
