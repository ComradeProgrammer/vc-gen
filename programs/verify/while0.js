function while0() {
    var x = 0; 
    var y = 1;
    while (y <= 5) {
        // invariant(x + 1 == y);
        invariant (x >= 0);
        invariant (y >= 1);
        x = y;
        y = y + 1;
    }
    assert(x >= 0);
}
