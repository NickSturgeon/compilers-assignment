PLATYPUS {
    f = -0.5;
    READ(i, t@);
    f = f + i;
    WRITE();
    WRITE(f);
    fout@ = "f is";
    IF TRUE (f < 0) THEN {
        fout@ = fout@ << " negative";
    } ELSE {
        fout@ = fout@ << " positive";
    };
    WRITE(fout@);
    s1@ = "_middle_";
    s2@ = "first" << s1@ << "last";
    WHILE TRUE (t@ <> "hello") REPEAT {
        !! Prompt the user to enter "hello"
        IF FALSE (t@ == "hello") THEN {
            WRITE("Please enter hello: ");
        } ELSE {
            WRITE("Never print me"); !! Should never display since
        };                           !! the loop will quit
        READ(t@);
    };
    WRITE();
    testlongname = 0;
    testlongname2 = 1;
    WRITE(s2@, testlongname); !! testlongname should print 1.000000
}
