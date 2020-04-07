#include <iostream>
#include <set>

#include "llvm/Support/raw_ostream.h"

#include "Interval.h"

using namespace llvm;
using namespace interval;

// Simple builder and accessor for pair representation
Interval interval::make(int l, int r) {
    return std::make_pair(l, r);
}
int interval::lower(Interval i) {
    return i.first;
}
int interval::upper(Interval i) {
    return i.second;
}

// Pre-defined intervals
Interval interval::full() {
    return make(minf, pinf);
}
Interval interval::empty() {
    return make(pinf, minf);
}
Interval interval::unit() {
    return make(0, 1);
}

/* Least Upper Bound
 *   Edge cases lead to extreme intervals or extreme bounds.
 *   General case takes the lowest of the lows and the highest of the highs
 *   as the bounds.
 */
Interval interval::lub(Interval l, Interval r) {
    Interval result;
    if (l == full()) {
        result = full();
    } else if (l == empty()) {
        result = r;
    } else if (lower(l) == minf && upper(r) == pinf) {
        result = full();
    } else if (lower(l) == minf) {
        result = make(minf, std::max(upper(l), upper(r)));
    } else if (upper(l) == pinf) {
        result = make(std::min(lower(l), lower(r)), pinf);
    } else {
        result = make(std::min(lower(l), lower(r)),
                      std::max(upper(l), upper(r)));
    }
    return result;
}

/* Unary negation
 *  Numerous special cases where the extreme bounds are involved.
 *  General case is to negate the bounds and use min/max to establish bounds.
 */
Interval interval::neg(Interval i) {
    Interval result;
    if (minf == lower(i) && pinf == upper(i)) {
        result = full();
    } else if (pinf == lower(i) && minf == upper(i)) {
        result = empty();
    } else if (minf == lower(i) && minf == upper(i)) {
        result = make(pinf, pinf);
    } else if (pinf == lower(i) && pinf == upper(i)) {
        result = make(minf, minf);
    } else if (pinf == upper(i)) {
        result = make(minf, -(lower(i)));
    } else if (minf == lower(i)) {
        result = make(-(upper(i)), pinf);
    } else {
        result = make(std::min(-(upper(i)), -(lower(i))),
                      std::max(-(upper(i)), -(lower(i))));
    }
    return result;
}

int checkOverflowAdd(int x, int y) {
    int z;
    if (x < 0 && y < 0) {
        z = minf - x;
        if (y <= z)
            return minf;
    } else if (x > 0 && y > 0) {
        z = pinf - x;
        if (y >= z)
            return pinf;
    }
    return x + y;
    
}

/* Addition
 *  Edge cases for empty intervals and maximal bounds
 *  General case is to add the corresponding bounds.
 */
Interval interval::add(Interval l, Interval r) {
    int low, up;
    if ((lower(l) == pinf && upper(l) == minf)
        || (lower(r) == pinf && upper(r) == minf)) //empty check
    {
        low = pinf;
        up = minf;
    } else {
        if (minf == lower(l) || minf == lower(r)) {
            low = minf;
        } else if(pinf == lower(l) || pinf == lower(r)){
            low = pinf;
        } else {
            low = checkOverflowAdd(lower(l), lower(r));
        }
        
        if (pinf == upper(l) || pinf == upper(r)) {
            up = pinf;
        } else if (minf == upper(l) || minf == upper(r)) {
            up = minf;
        } else {
            up = checkOverflowAdd(upper(l), upper(r));
        }
        
    }
    return make(low, up);
}

Interval interval::sub(Interval l, Interval r) {
    return interval::add(l, interval::neg(r));
}

int mulOverflow(int x, int y) {
    if (x > 0 && y > 0) {
        int k = pinf / x;
        if (y > k)
            return pinf;
    } else if (x < 0 && y < 0) {
        if (x == minf || y == minf)
            return pinf;
        int k = pinf / x;
        if (y < k)
            return pinf;
    } else if (x < 0 && y > 0) {
        int k = minf / x;
        if (y > k)
            return minf;
    } else if (x > 0 && y < 0) {
        int k = minf / y;
        if (x > k)
            return minf;
    }
    return x * y;
}

/* Multiplication
 */
Interval interval::mul(Interval l, Interval r) {
    int low, up;
    
    if ((lower(l) == pinf && upper(l) == minf)
        || (lower(r) == pinf && upper(r) == minf)) {
        low = pinf;
        up = minf;
    } else {
        low = std::min(
                       std::min(mulOverflow(lower(l), lower(r)),
                                mulOverflow(lower(l), upper(r))),
                       std::min(mulOverflow(upper(l), lower(r)),
                                mulOverflow(upper(l), upper(r))));
        up = std::max(
                      std::max(mulOverflow(lower(l), lower(r)),
                               mulOverflow(lower(l), upper(r))),
                      std::max(mulOverflow(upper(l), lower(r)),
                               mulOverflow(upper(l), upper(r))));
    }
    return make(low, up);
}

Interval mulD(Interval l, std::pair<double, double> d) {
    double lr1 = lower(l) * d.first;
    double lr2 = lower(l) * d.second;
    double lr3 = upper(l) * d.first;
    double lr4 = upper(l) * d.second;
    double lowD = std::round(std::min(std::min(lr1, lr2), std::min(lr3, lr4)));
    double upD = std::round(std::max(std::max(lr1, lr2), std::max(lr3, lr4)));
    int low = lowD, up = upD;
    if (lowD < minf) {
        low = minf;
    } else if (lowD > pinf) {
        low = pinf;
    }
    if (upD < minf) {
        up = minf;
    } else if (upD > pinf) {
        up = pinf;
    }
    return make(low, up);
}

/* Division
 */
Interval interval::div(Interval l, Interval r) {
    //overflow handles by mul
    //empty
    if ((lower(l) == pinf && upper(l) == minf)
        || (lower(r) == pinf && upper(r) == minf)) {
        return make(pinf, minf);
    }
    
    //0 not in range
    if ((lower(r) < 0 && upper(r) < 0) || ((lower(r) > 0 && upper(r) > 0))) {
        return mulD(l, std::make_pair((1.0 / upper(r)), (1.0 / lower(r))));
    }
    
    //0 checking
    if (upper(r) == 0) {
        return mulD(l, std::make_pair(minf, (1.0 / lower(r))));
    }
    
    //0 checking
    if (lower(r) == 0) {
        return mulD(l, std::make_pair((1.0 / upper(r)), pinf));
    }
    
    //0 in range
    return mul(l, make(minf, pinf));
}

/* Comparison Operators
 *   Trivial imprecise definitions
 */
Interval interval::lt(Interval l, Interval r) {
    if ((lower(l) == pinf && upper(l) == minf)
        || (lower(r) == pinf && upper(r) == minf)) {
        return unit();
    }
    if (upper(l) < lower(r)) {
        return make(1, 1);
    } else if (lower(l) > upper(r)) {
        return make(0, 0);
    }
    return unit();
}
Interval interval::gt(Interval l, Interval r) {
    if ((lower(l) == pinf && upper(l) == minf)
        || (lower(r) == pinf && upper(r) == minf)) {
        return unit();
    }
    if (upper(l) < lower(r)) {
        return make(0, 0);
    } else if (lower(l) > upper(r)) {
        return make(1, 1);
    }
    return unit();
}
Interval interval::eq(Interval l, Interval r) {
    if ((lower(l) == pinf) || (lower(l) == minf))
        return unit();
    if ((lower(r) == pinf) || (lower(r) == minf))
        return unit();
    if ((upper(l) == pinf) || (upper(l) == minf))
        return unit();
    if ((upper(r) == pinf) || (upper(r) == minf))
        return unit();
    
    if ((lower(l) == lower(r)) && (upper(l) == upper(r))
        && (lower(l) == upper(l)))
        return make(1, 1);
    
    if ((upper(l) < lower(r)) || (lower(l) > upper(r)))
        return make(0, 0);
    
    return unit();
}
Interval interval::ne(Interval l, Interval r) {
    if ((lower(l) == pinf) || (lower(l) == minf))
        return unit();
    if ((lower(r) == pinf) || (lower(r) == minf))
        return unit();
    if ((upper(l) == pinf) || (upper(l) == minf))
        return unit();
    if ((upper(r) == pinf) || (upper(r) == minf))
        return unit();
    
    if ((lower(l) == lower(r)) && (upper(l) == upper(r))
        && (lower(l) == upper(l)))
        return make(0, 0);
    
    if ((upper(l) < lower(r)) || (lower(l) > upper(r)))
        return make(1, 1);
    
    return unit();
}

std::string istr(int b) {
    std::string result = "";
    if (b == minf) {
        result = "-inf";
    } else if (b == pinf) {
        result = "+inf";
    } else {
        result = std::to_string(b);
    }
    return result;
}

std::string interval::str(Interval i) {
    std::string f = istr(lower(i));
    std::string s = istr(upper(i));
    return "[" + f + "," + s + "]";
}

// Deep equality for intervals
bool interval::operator==(Interval l, Interval r) {
    return (lower(l) == lower(r)) && (upper(l) == upper(r));
}

bool interval::operator!=(Interval l, Interval r) {
    return !(l == r);
}
