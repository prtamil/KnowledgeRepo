function process(str){
        var alpabet = 'abcdefghijklmnopqrstuvwxyz';
        var st = str.toLowerCase();
        var sstr = [].slice.call(st);
        var al   = [].slice.call(alpabet);
        var res = al.every(function(eel){
                return sstr.some(function(sel){
                        return eel === sel;
                });
        });
        if(res)
                return 'pangram';
        else
                return 'not pangram';
}

process('We promptly judged antique ivory buckles for the next prize')

//Every thing in a array should be present in some of b
al.every(el=>b.some(bl=> el === bl));
//Everything in a array should be present in every
