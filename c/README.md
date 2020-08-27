# MiniForth

A mini compiler which compiles the Forth like code into bitcoin opcodes.

## Build from source

1. have erlang and elixir installed in your machine, recommend using [asdf](https://asdf-vm.com/#/) to install 
2. Clone this repo, cd into the directory.
3. `cd c`
4. `mix deps.get`
5. `MIX_ENV=prod mix escript.build`

## How to use

Create a new file `hello.fth` , and write:

```fth
: main "hello" drop ;
```

Run `./mini_forth hello.fth`. Then the terminal will print:

```
[RAW SCRIPT]
"68656c6c6f OP_DROP"

[EVAL RESULT]
MainStack: []
AltStack:  []
```

There're some code examples in `/example` directory.

## Data and opcodes

### Constants

| MiniForth code       | Bitcoin Sciprt ( can be read by bsv.js ) | Description      |
| -------------------- | ---------------------------------------- | ---------------- |
| 0 1 2 3              | OP_0 OP_1 OP_2 OP_3                      | Numbers          |
| 0xFF 0xff 0x1234     | ff00 ff00 3412                           | Hex Numbers      |
| -1 -2 -3             | OP_1NEGATE 82 83                         | Negative Numbers |
| "hello world"        | 68656c6c6f20776f726c64                   | ASCII String     |
| <<0, 1, 2, 3, 0x04>> | 0001020304                               | Bytes            |

### Arithmetic

| MiniForth code  | Bitcoin Sciprt ( can be read by bsv.js )                     | Description |
| --------------- | ------------------------------------------------------------ | ----------- |
| + - * / %       | OP_ADD OP_SUB OP_MUL OP_DIV OP_MOD                           |             |
| 1-              | OP_1SUB                                                      |             |
| num=            | OP_NUMEQUAL                                                  |             |
| num=verify      | OP_NUMEQUALVERIFY                                            |             |
| and or not not0 | OP_BOOLAND OP_BOOLOR OP_NOT OP_0NOTEQUAL                     |             |
| >= <= > <       | OP_GREATERTHANOREQUAL OP_LESSTHANOREQUAL OP_GREATERTHAN OP_LESSTHAN |             |

### Bitwise Logic

| MiniForth code | Bitcoin Sciprt ( can be read by bsv.js ) | Description |
| -------------- | ---------------------------------------- | ----------- |
| =              | OP_EQUAL                                 |             |
| =verify        | OP_EQUALVERIFY                           |             |
| & \| ^ ~       | OP_AND OP_OR OP_XOR OP_INVERT            |             |

### Unimplemented

- OP_CHECKSIG
- OP_CHECKSIGVERIFY
- OP_CHECKMULTISIG
- OP_CHECKMULTISIGVERIFY

### Other Opcodes

Other opcodes are expressed with downcase words, for example: `drop swap` stands for `OP_DROP OP_SWAP`.

### Debug Operators

| MiniForth code | Bitcoin Sciprt ( can be read by bsv.js ) | Description           |
| -------------- | ---------------------------------------- | --------------------- |
| .              | ignore                                   | Print the top element |
| cr             | ignore                                   | Print a new line      |

## Syntax

The syntax is basically a simplified version of Forth language.

### Define new word

```fth
: 3sub 3 - ;
```

### Comment

```fth
\ a line starts with '\' is comment

\ we use ( a -- b ) to comment the arguments and returns of a word
: left ( str n -- left_str )
    split drop ;
```

## Compile time macros

### Loop

```fth
\ the loop will be unroll at compile time
: dup3times ( -- )
    3 0 do dup loop ;  
\ will be compile into `OP_DUP OP_DUP OP_DUP`

\ can use the +loop to specify the step
: countdown 
    0 10 do cr i . -1 +loop ;
```

### literal

```fth
\ the code inside a `[ ]` is a quote
\ when the compiler see `literal`, it will execute nearest quote
\ and place the result at the position of `literal`

\ this code equal to `: two 2 ;`
: two [ 1 1 + ] literal ;
```

### bi, tri

These functions are borrowed from the [Factor](https://factorcode.org/) language.

```fth
\ equal to `1 1 + 1 2 *`
: main 1 [ 1 + ] [ 2 * ] bi ;

\ equal to `1 1 + 1 2 + 1 3 +`
: main 1 [ 1 + ] [ 2 + ] [ 3 + ] tri ;
```

## Support

If you have any questions or errors during the use, please raise an issue or ask questions on the [Bitcoin Script channel](https://powping.com/c/a4d2d4ce12cfba93342a8f7eebbdd91ebe2689d68ba98fa340f457e89104b3cb) of Powping.

