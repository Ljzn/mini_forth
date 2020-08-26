# MiniForth



## Data and Opcodes

### Constants

| MiniForth code       | Bitcoin Sciprt ( can be read by bsv.js ) | Description      |
| -------------------- | ---------------------------------------- | ---------------- |
| 0 1 2 3              | OP_0 OP_1 OP_2 OP_3                      | Numbers          |
| 0xFF 0xff 0x1234     | ff00 ff00 3412                           | Hex Numbers      |
| -1 -2 -3             | OP_1NEGATE 82 83                         | Negative Numbers |
| "hello world"        | 68656c6c6f20776f726c64                   | ASCII String     |
| <<0, 1, 2, 3, 0x04>> | 0001020304                               | Bytes            |

### Arithmetic

| MiniForth code | Bitcoin Sciprt ( can be read by bsv.js )                     | Description |
| -------------- | ------------------------------------------------------------ | ----------- |
| + - * / %      | OP_ADD OP_SUB OP_MUL OP_DIV OP_MOD                           |             |
| 1-             | OP_1SUB                                                      |             |
| num=           | OP_NUMEQUAL                                                  |             |
| num=verify     | OP_NUMEQUALVERIFY                                            |             |
| 1 2 and 0 or   | OP_1 OP_2 OP_BOOLAND OP_0 OP_BOOLOR                          |             |
| >= <= > <      | OP_GREATERTHANOREQUAL OP_LESSTHANOREQUAL OP_GREATERTHAN OP_LESSTHAN |             |

### Bitwise Logic

| MiniForth code   | Bitcoin Sciprt ( can be read by bsv.js )          | Description |
| ---------------- | ------------------------------------------------- | ----------- |
| =                | OP_EQUAL                                          |             |
| =verify          | OP_EQUALVERIFY                                    |             |
| 1 2 & 0 \| 1 ^ ~ | OP_1 OP_2 OP_AND OP_0 OP_OR OP_1 OP_XOR OP_INVERT |             |

### Unimplemented

- OP_CHECKSIG
- OP_CHECKSIGVERIFY
- OP_CHECKMULTISIG
- OP_CHECKMULTISIGVERIFY

### Other Opcodes

Other opcodes are expressed with downcase words, for example: `drop swap` stands for `OP_DROP OP_SWAP`.

