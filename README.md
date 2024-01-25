### Requirements

You will need an [arkworks-bridge](https://github.com/torsion-labs/arkworks-bridge/releases) binary in your path in order to generate the verifying smart contract and proof.

We use [cliquebait](https://github.com/f-o-a-m/cliquebait) to run the tests. You can use hardhat or foundry/anvil if that suits your needs.

### Generate CRS and smart contract for R1CS

```
> arkworks-bridge create-trusted-setup --r1cs ./proof-data/sudoku-r1cs.jsonl --proving-key ./proof-data/sudoku-pk --verifying-key ./proof-data/sudoku-vk --ethereum
> arkworks-bridge generate-contract --contract ./contracts/Verifier.sol --verifying-key ./proof-data/sudoku-vk --inputs ./proof-data/sudoku-assignments.jsonl
```

### Quickstart

```
> npm i
> npm run chanterelle-build
> arkworks-bridge create-proof --proof ./proof-data/sudoku-proof --proving-key ./proof-data/sudoku-pk --r1cs ./proof-data/sudoku-r1cs.jsonl --witness ./proof-data/sudoku-witness.jsonl --ethereum
> docker run --rm -it -p 8545:8545 -d foamspace/cliquebait:latest
> npm run deploy
```
