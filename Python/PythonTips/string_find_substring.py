"Pythonic Way"
substring in mainstring  => True or False
"in keyword works"

"Normal Way"
def findsubstr(ms,ss):
    cnt = 0
    for i in range(len(ms)):
        if ms[i:i+len(ss)] == ss:
            cnt += 1
    print(ss,'found ',str(cnt))
    return cnt

""" 
 //C 
  char* strstr(char* haystack, char* needle) {
    for (;; ++haystack) {
        char* h = haystack;
        for (char* n = needle;; ++n, ++h) {
            if (!*n) return haystack;
            if (*h != *n) break;
        }
        if (!*h) return NULL;
    }
}


//Javascript
function subs(ms,ss){
        var cnt = 0;
        for(var i = 0; i < ms.length; i++){
                
                if(ms.slice(i,i+ss.length) === ss){
                        cnt ++;
                }
        }
        console.log('times found',cnt);
}