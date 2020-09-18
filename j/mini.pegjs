/*
Parsing the MiniForth code into the ASM string of bitcoin opcodes.
Copyright (C) 2020  Venezia(390@moneybutton.com)

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/

{
	function alias(op) {
    	op = op.replace(/^WORD:/g, '');
    	let full = {
        	tas: 'toaltstack',
            fas: 'fromaltstack',
            '+': 'add',
            '-': 'sub',
            '*': 'mul',
            '/': 'div'
        }[op];
        if(full) {
        	return 'OP_' + full.toUpperCase()
        }
    }

	function toASM(dict) {
    	let main = dict.get("WORD:main");
        main.reverse();
        let asm = '';
        console.log(main)
        while(main.length > 0) {
        	let h = main.pop();
            let s;
            if(typeof(h) == 'bigint') {
            	s = minimal_encode(h);
            } else if(alias(h)){
            	s = alias(h);
            } else if(h.startsWith('WORD:')) {
            	s = h.replace(/^WORD:/g, 'OP_').toUpperCase();
            } else {
            	s = h;
            }
            asm = asm + ' ' + s;
        }
        console.log(asm);
        return asm.trimStart();
    }
	function unroll(dict) {
    	let main = dict.get("WORD:main");
        if(!main) {
        	error("Must have a 'main' word defined")
        }
       	main = main.reverse();
        var m1 = [];
        while(main.length > 0) {
        	let h = main.pop();
            if(h == 'WORD:do') {
            	let start = m1.pop();
                let stop = m1.pop();
				if(!(typeof(start) == 'bigint' && typeof(stop) == 'bigint')) {
                	error('The start and stop params of loop must be known at compile time')
                }
                var loop = [];
                var step = 1;
                while(main.length > 0) {
                	let h = main.pop();
                    if(h == 'WORD:loop') {
                        break;
                    } else {
                    	loop.push(h);
                    }
                }
                for(let i = start; i < stop; i++) {
                	let loop1 = JSON.parse(JSON.stringify(loop));
                	for(let j = 0; j < loop.length; j++) {
                        if(loop[j] == 'WORD:i') {
                        	loop1[j] = i;
                        }
                    }
                	m1 = m1.concat(loop1);
                }
            } else {
            	m1.push(h);
            }
        }

        dict.set('WORD:main', m1);

        return dict
    }

	function replace(dict) {
    	var modified = false;

    	// do replace
        dict.forEach((v, k) => {
            // console.log([v, k])
            if(v.includes(k)) {
                error("Recursion word '" + k + "' is unsupported")
            }

        	for(var i = 0; i < v.length; i++) {
            	let a = dict.get(v[i]);
                if(a) {
                	v[i] = a;
             		v = v.flat();
                    dict.set(k, v);
                    modified = true;
                    break;
                }
            }
        })

    	if(!modified) {
        	return dict;
        } else {
        	return replace(dict);
        }
    }

	function stringToHex(str) {
      var result = '';
      for (var i=0; i<str.length; i++) {
        result += str.charCodeAt(i).toString(16);
      }
      return result;
    }

	function minimal_encode(v) {
    	if(v >= 0 && v <=16) {
        	return 'OP_' + v;
        }
    	let signBit = v > 0 ? 0 : 0x80;
        v = v > 0 ? v : -v;
        var result = '';
        for (var i = 0; i < 1000; i ++) {
           let e;
           if(v < 128) {
               e = Number(v)|signBit
               v = 'end';
           } else {
               e = v % 256n;
               v = v/256n;
           }
           result += e.toString(16).padStart(2, '0');
           if(v == 'end') {
           	return result;
           }
        }
   	 	error('The number is too large: ' + v);
    }

    function num2bin(v, bits, BE, unsigned) {
        if(bits % 8n) {
            error('The bits size should be multiples of 8')
        }
        let max = 2n**bits - 1n;
        if(v > max || v < -max) {
            error('Impossible encoding for number ' + v)
        }
        let signBit = v > 0n ? 0n : 0x80n;
        v = v >= 0n ? v : -v
        let bytes = bits / 8n;
        var result = '';
        for (var i = 0; i < bytes; i ++) {
           let e;
           if(!unsigned && i == bytes-1) {
               e = Number(v % 128n)|signBit
           } else {
               e = Number(v % 256n);
               v = v/256n;
           }
           if(!BE) {
           	result += e.toString(16).padStart(2, '0');
           } else {
           	result = e.toString(16).padStart(2, '0') + result;
           }
        }
        return result;
    }
}

start
	= ws head:block tail:blockTail* ws {
    	tail.unshift(head);
        let r = new Map();
        for (var i = 0; i < tail.length; i++) {
        	if(tail[i]) {
        		let k = tail[i].name;
            	r.set(k, tail[i].body);
            }
        }
        // console.log(r)
        r = replace(r)
        // console.log(r)
        r = unroll(r)
        // console.log(r)
        r = toASM(r)
        return r;
      }

word "word"
  = w:$(digit* letter ( digit / letter )*) { return 'WORD:' + w }
  / [\+\-\*\/]

letter =
	[a-z]i / '_'

digit =
	[0-9]

block
	= definition
    / comment

blockTail
	= ws b:block { return b }

elements
	= tail:elementTail* {
    	var r = [];
    	for(var i = 0; i < tail.length; i++) {
        	if(tail[i] != null) {
            	r.push(tail[i])
            }
        }
        return r;
      }

definition
	= ':' ws w:word e:elements ws ';' { return { name: w, body: e } }

LineTerminator
 	= [\n\r\u2028\u2029]

element
	= bytes
    / word
    / number
    / string
    / comment

elementTail
	= ws e:element { return e; }

string "string"
  = quotation_mark chars:char* quotation_mark { return stringToHex(chars.join("")); }

char
  = unescaped
  / escape
    sequence:(
        '"'
      / "\\"
      / "/"
      / "b" { return "\b"; }
      / "f" { return "\f"; }
      / "n" { return "\n"; }
      / "r" { return "\r"; }
      / "t" { return "\t"; }
      / "u" digits:$(HEXDIG HEXDIG HEXDIG HEXDIG) {
          return String.fromCharCode(parseInt(digits, 16));
        }
    )
    { return sequence; }

escape
  = "\\"

quotation_mark
  = '"'

unescaped
  = [^\0-\x1F\x22\x5C]

HEXDIG = [0-9a-f]i

comment
	= bracketsComment
    / singleLineComment

singleLineComment
	= '\\' (!LineTerminator .)*  { return null }

bracketsComment
	= '(' (!')' .)* ')' { return null }

bytes
    = '<<' ws head:segment tail:segmentTail* '>>' {
    	tail.unshift(head);
        return tail.reduce(
        	(acc, x) => {
        		return acc + num2bin(x.value, x.size, true, true)
        	}, ''
        );
      }
     / '<<>>' { return 0n }

segmentTail
    = ws ',' ws seg:segment { return seg; }

segment
    = v:number size:size ?
      { return {value: v, size: size || 8}; }

hexDigit
  = [0-9a-fA-F]

number
	= posNumber
    / negNumber

posNumber
    = hexNumber
    / '0' { return 0n; }
    / head:[1-9] tail:[0-9]* {
    	var x;
    	x = BigInt(head + tail.join(''));
        return x;
      }

negNumber
	= '-' n:posNumber { return -n }

hexNumber
    = '0x' head:hexDigit tail:hexDigit* { return BigInt('0x' + head + tail.join(''), 16); }

size
    = ':' num:number { return num; }

ws = [ \t\n]*