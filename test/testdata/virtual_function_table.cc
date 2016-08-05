// virtual members
#include <iostream>
using namespace std;

class Polygon {
protected:
    int width, height;
public:
    void set_values (int a, int b)
    { width=a; height=b; }
    virtual int area ()
    { return 0; }
};

class Rectangle: public Polygon {
public:
    int area ()
    { return width * height; }
};

class Triangle: public Polygon {
public:
    int area ()
    { return (width * height / 2); }
};

__attribute__ ((noinline)) void printArea(Polygon *p) {
    p->set_values(4,5);
    cout << p->area() << '\n';
}

void (*f)(Polygon *p);

int main () {
    Rectangle rect;
    Triangle trgl;
    Polygon poly;
    printArea(&rect);
    printArea(&trgl);
    printArea(&poly);
    f = printArea;
    f(&rect);
    return 0;
}
