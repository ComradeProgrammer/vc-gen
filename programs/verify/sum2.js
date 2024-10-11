function sum(n) {
    var i = 1; 
    var sum = 0;  
    while (n >= i) {
        invariant (sum>=0);
        var j = 1;
        while (i >= j) {
            invariant (j>=1);
            invariant (sum>=0);
            sum = sum + j;
            j = j + 1;
            i = i + 1;
        }
        assert (sum >= 0);
    }
}


