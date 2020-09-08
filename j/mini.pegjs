{
	function replace(dict) {
    	var modified = false;
       
    	// do replace
        dict.forEach((v, k) => {
        	for(var i = 0; i < v.length; i++) {
            	let a = dict.get(v[i]);
                if(a) {
                	v[i] = a;
             		v = v.flat();
                    dict.set(k, v);
                    modified = true;
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
        v = Math.abs(v);
        var result = '';
        for (var i = 0; i < 1000; i ++) {
           let e;
           if(v < 128) {
               e = v|signBit
               v = 'end';
           } else {
               e = (v % 256);
               v = Math.floor(v/256);
           }
           result += e.toString(16).padStart(2, '0');
           if(v == 'end') {
           	return result;
           }
        }
   		return 'The number is too large: ' + v;
    }
    
    function num2bin(v, bits, BE, unsigned) { 
        if(bits % 8) {
            return 'The bits size should be multiples of 8'
        }
        let max = 2**bits - 1;
        if(v > max || v < -max) {
            return 'Impossible encoding for number ' + v
        }
        let signBit = v > 0 ? 0 : 0x80;
        v = Math.abs(v);
        let bytes = bits / 8;
        var result = '';
        for (var i = 0; i < bytes; i ++) {
           let e;
           if(!unsigned && i == bytes-1) {
               e = (v % 128)|signBit
           } else {
               e = (v % 256);
               v = Math.floor(v/256);
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
	= ws head:block tail:blockTail* {
    	tail.unshift(head);
        let r = new Map();
        for (var i = 0; i < tail.length; i++) {
        	if(tail[i]) {
        		let k = tail[i].name;
            	r.set(k, tail[i].body);
            }
        }
        return console.log(replace(r));
      }

word "word"
	= chars:nameChar+ { return 'WORD:' + chars.join('') }
    
nameChar
    = [a-z0-9]

block
	= definition
    / comment
    
blockTail
	= ws b:block { return b }

elements
	= tail:elementTail* {
        return tail;
      }
    
definition
	= ':' ws w:word e:elements ' ;' { return { name: w, body: e } }
    
LineTerminator
 	= [\n\r\u2028\u2029]

element
	= bytes
    / number
    / string
    / word
    
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

segmentTail
    = ws ',' ws seg:segment { return seg; }

segment
    = v:number size:size ?
      { return {value: v, size: size || 8}; }

hexDigit
  = [0-9a-fA-F]

number
    = hexNumber
    / '0' { return 0; }
    / head:[1-9] tail:[0-9]* {
    	var x;
    	x = parseInt(head + tail.join(''));
        return x;
      }
    
hexNumber
    = '0x' head:hexDigit tail:hexDigit* { return parseInt(head + tail.join(''), 16); }

size
    = ':' num:number { return num; }

ws = [ \t\n]*