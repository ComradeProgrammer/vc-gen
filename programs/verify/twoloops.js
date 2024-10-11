function twoloops(n) {
    var i = 0;
    while (i <= 3) {
        invariant (i <= 4);
        
        i = i + 1;
    }
    while (i <= 7) {
        invariant (i <= 8);

        i = i + 1;
    }
    assert (i == 8);
}
