import std.format;
import std.stdio;

struct A {
    B b1;
    B* b2;
}

struct B {
    int i;
}

void main() {
    B b = { i: 5 };
    const A a = { b1: b, b2: &b };
    a.b1.i += 1;
    a.b2.i += 2;
    "%d %d".format(a.b1.i, a.b2.i).writeln();
}
