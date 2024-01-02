/** @type import('hardhat/config').HardhatUserConfig */
require("hardhat-tracer");

module.exports = {
    solidity: "0.8.19",
    defaultNetwork: "hardhat",
    networks: {
        hardhat: {
            blockGasLimit: 1000000000000000, // whatever you want here
	    initialBaseFeePerGas: 1,
            gasPrice: 1,
        },
    }
}
