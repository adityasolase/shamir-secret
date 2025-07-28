#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <algorithm>
#include <cctype>
#include <cassert>

using namespace std;

// Minimal BigInt (base 1e9)
struct BigInt {
    static const int BASE = 1000000000;
    int sign;                 
    vector<int> a;            

    BigInt(): sign(0) {}

    BigInt(long long v) { *this = v; }

    BigInt& operator=(long long v) {
        a.clear();
        if (v == 0) { sign = 0; return *this; }
        sign = (v < 0 ? -1 : 1);
        unsigned long long x = (v < 0 ? -v : v);
        while (x) {
            a.push_back((int)(x % BASE));
            x /= BASE;
        }
        return *this;
    }

    bool isZero() const { return sign == 0; }

    static int cmpAbs(const BigInt &A, const BigInt &B) {
        if (A.a.size() != B.a.size()) return (A.a.size() < B.a.size() ? -1 : 1);
        for (int i = (int)A.a.size() - 1; i >= 0; --i) {
            if (A.a[i] != B.a[i]) return (A.a[i] < B.a[i] ? -1 : 1);
        }
        return 0;
    }

    static BigInt addAbs(const BigInt &A, const BigInt &B) {
        BigInt res;
        res.sign = 1;
        int carry = 0;
        size_t n = max(A.a.size(), B.a.size());
        res.a.resize(n, 0);
        for (size_t i = 0; i < n || carry; ++i) {
            if (i == res.a.size()) res.a.push_back(0);
            long long cur = carry;
            if (i < A.a.size()) cur += A.a[i];
            if (i < B.a.size()) cur += B.a[i];
            carry = (int)(cur >= BASE);
            if (carry) cur -= BASE;
            res.a[i] = (int)cur;
        }
        res.trim();
        return res;
    }

    static BigInt subAbs(const BigInt &A, const BigInt &B) {
        
        BigInt res;
        res.sign = 1;
        res.a = A.a;
        int carry = 0;
        for (size_t i = 0; i < B.a.size() || carry; ++i) {
            long long cur = res.a[i] - (long long)(i < B.a.size() ? B.a[i] : 0) - carry;
            carry = (cur < 0);
            if (carry) cur += BASE;
            res.a[i] = (int)cur;
        }
        res.trim();
        return res;
    }

    BigInt operator-() const {
        BigInt r = *this;
        if (!r.isZero()) r.sign = -r.sign;
        return r;
    }

    BigInt operator+(const BigInt &v) const {
        if (isZero()) return v;
        if (v.isZero()) return *this;
        if (sign == v.sign) {
            BigInt r = addAbs(*this, v);
            r.sign = sign;
            if (r.isZero()) r.sign = 0;
            return r;
        }
        int cmp = cmpAbs(*this, v);
        if (cmp == 0) {
            return BigInt(0);
        } else if (cmp > 0) {
            BigInt r = subAbs(*this, v);
            r.sign = sign;
            if (r.isZero()) r.sign = 0;
            return r;
        } else {
            BigInt r = subAbs(v, *this);
            r.sign = v.sign;
            if (r.isZero()) r.sign = 0;
            return r;
        }
    }

    BigInt operator-(const BigInt &v) const {
        return *this + (-v);
    }

    BigInt& operator+=(const BigInt &v) {
        *this = *this + v;
        return *this;
    }

    BigInt& operator-=(const BigInt &v) {
        *this = *this - v;
        return *this;
    }

    
    BigInt mulInt(long long m) const {
        if (m == 0 || isZero()) return BigInt(0);
        BigInt res;
        res.sign = (sign == 0 ? 0 : (m < 0 ? -sign : sign));
        unsigned long long mm = (m < 0 ? -m : m);
        unsigned long long carry = 0;
        res.a.resize(a.size());
        for (size_t i = 0; i < a.size(); ++i) {
            unsigned long long cur = carry + (unsigned long long)a[i] * mm;
            res.a[i] = (int)(cur % BASE);
            carry = cur / BASE;
        }
        while (carry) {
            res.a.push_back((int)(carry % BASE));
            carry /= BASE;
        }
        res.trim();
        return res;
    }

    // divide by a positive small integer, return quotient 
    BigInt divInt(long long d, long long *remainder_out = nullptr) const {
        assert(d > 0);
        if (isZero()) {
            if (remainder_out) *remainder_out = 0;
            return BigInt(0);
        }
        BigInt res;
        res.sign = sign;
        unsigned long long dd = (unsigned long long)d;
        unsigned long long rem = 0;
        res.a.resize(a.size());
        for (int i = (int)a.size() - 1; i >= 0; --i) {
            unsigned long long cur = a[i] + rem * (unsigned long long)BASE;
            res.a[i] = (int)(cur / dd);
            rem = cur % dd;
        }
        if (remainder_out) *remainder_out = (long long)rem;
        res.trim();
        return res;
    }

    
    BigInt addInt(long long v) const {
        BigInt B(v);
        return *this + B;
    }

    string toString() const {
        if (sign == 0) return "0";
        string s = (sign < 0 ? "-" : "");
        int i = (int)a.size() - 1;
        s += to_string(a[i]);
        for (i = i - 1; i >= 0; --i) {
            string block = to_string(a[i]);
            // pad leading zeros
            if ((int)block.size() < 9) s += string(9 - block.size(), '0');
            s += block;
        }
        return s;
    }

private:
    void trim() {
        while (!a.empty() && a.back() == 0) a.pop_back();
        if (a.empty()) sign = 0;
    }
};


int charToVal(char c) {
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'a' && c <= 'z') return 10 + (c - 'a');
    if (c >= 'A' && c <= 'Z') return 10 + (c - 'A');
    return -1;
}

BigInt decodeInBase(const string &s, int base) {
    BigInt res(0);
    for (char c : s) {
        int d = charToVal(c);
        if (d < 0 || d >= base) {
            
            continue;
        }
        
        res = res.mulInt(base);
        res = res.addInt(d);
    }
    return res;
}

// JSON parsing


struct Point {
    long long x;
    BigInt y;
};

struct TestCase {
    int n, k;
    vector<Point> points;
};

string readFile(const string &path) {
    ifstream fin(path);
    if (!fin) {
        cerr << "Cannot open file: " << path << "\n";
        exit(1);
    }
    string all, line;
    while (getline(fin, line)) all += line;
    return all;
}


void skipSpaces(const string &s, size_t &i) {
    while (i < s.size() && isspace((unsigned char)s[i])) ++i;
}

bool matchChar(const string &s, size_t &i, char c) {
    skipSpaces(s, i);
    if (i < s.size() && s[i] == c) { ++i; return true; }
    return false;
}

string parseQuotedString(const string &s, size_t &i) {
    skipSpaces(s, i);
    if (i >= s.size() || s[i] != '"') {
        cerr << "JSON parse error: expected quote\n";
        exit(1);
    }
    ++i;
    string out;
    while (i < s.size() && s[i] != '"') {
        out.push_back(s[i++]);
    }
    if (i == s.size()) {
        cerr << "JSON parse error: unterminated string\n";
        exit(1);
    }
    ++i; 
    return out;
}

long long parseInt(const string &s, size_t &i) {
    skipSpaces(s, i);
    bool neg = false;
    if (i < s.size() && (s[i] == '-' || s[i] == '+')) {
        neg = (s[i] == '-');
        ++i;
    }
    long long val = 0;
    if (i >= s.size() || !isdigit((unsigned char)s[i])) {
        cerr << "JSON parse error: expected integer\n";
        exit(1);
    }
    while (i < s.size() && isdigit((unsigned char)s[i])) {
        val = val * 10 + (s[i] - '0');
        ++i;
    }
    return neg ? -val : val;
}

TestCase parseTestCase(const string &jsonText) {
    TestCase tc;
    size_t i = 0;
    skipSpaces(jsonText, i);
    if (!matchChar(jsonText, i, '{')) {
        cerr << "JSON parse error: expected '{'\n";
        exit(1);
    }

    while (true) {
        skipSpaces(jsonText, i);
        if (matchChar(jsonText, i, '}')) {
            break; 
        }

        string key = parseQuotedString(jsonText, i);
        if (!matchChar(jsonText, i, ':')) {
            cerr << "JSON parse error: expected ':'\n";
            exit(1);
        }

        if (key == "keys") {
            if (!matchChar(jsonText, i, '{')) {
                cerr << "JSON parse error: expected '{' for keys\n";
                exit(1);
            }
            
            bool gotN = false, gotK = false;
            while (true) {
                skipSpaces(jsonText, i);
                if (matchChar(jsonText, i, '}')) break;
                string kk = parseQuotedString(jsonText, i);
                if (!matchChar(jsonText, i, ':')) {
                    cerr << "JSON parse error: expected ':' inside keys\n";
                    exit(1);
                }
                long long val = parseInt(jsonText, i);
                if (kk == "n") { tc.n = (int)val; gotN = true; }
                else if (kk == "k") { tc.k = (int)val; gotK = true; }
                skipSpaces(jsonText, i);
                matchChar(jsonText, i, ','); 
            }
            if (!(gotN && gotK)) {
                cerr << "JSON parse error: keys missing n or k\n";
                exit(1);
            }
        } else {
            
            long long x = 0;
            {
                
                bool ok = true;
                for (char c : key) if (!isdigit((unsigned char)c)) ok = false;
                if (!ok) {
                    cerr << "JSON parse error: unexpected key '" << key << "'\n";
                    exit(1);
                }
                x = stoll(key);
            }

            if (!matchChar(jsonText, i, '{')) {
                cerr << "JSON parse error: expected '{' for point\n";
                exit(1);
            }

            
            int base = 10;
            string valstr;
            bool gotBase = false, gotValue = false;
            while (true) {
                skipSpaces(jsonText, i);
                if (matchChar(jsonText, i, '}')) break;
                string kk = parseQuotedString(jsonText, i);
                if (!matchChar(jsonText, i, ':')) {
                    cerr << "JSON parse error: expected ':' inside point\n";
                    exit(1);
                }
                if (kk == "base") {
                    string baseStr = parseQuotedString(jsonText, i);
                    base = stoi(baseStr);
                    gotBase = true;
                } else if (kk == "value") {
                    valstr = parseQuotedString(jsonText, i);
                    gotValue = true;
                } else {
                    
                    (void)parseQuotedString(jsonText, i);
                }
                skipSpaces(jsonText, i);
                matchChar(jsonText, i, ','); 
            }
            if (!(gotBase && gotValue)) {
                cerr << "JSON parse error: point missing base or value\n";
                exit(1);
            }

            Point p;
            p.x = x;
            p.y = decodeInBase(valstr, base);
            tc.points.push_back(p);
        }

        skipSpaces(jsonText, i);
        matchChar(jsonText, i, ','); 
    }

    return tc;
}

// Lagrange interpolation @ x=0 

BigInt interpolate_constant_term(vector<Point> points, int k) {
    // take first k points (sorted by x for determinism)
    sort(points.begin(), points.end(), [](const Point &a, const Point &b){ return a.x < b.x; });
    points.resize(k);

    BigInt ans(0);

    for (int i = 0; i < k; ++i) {
        long long num = 1;    
        long long den = 1;    
        for (int j = 0; j < k; ++j) {
            if (j == i) continue;
            num *= -points[j].x;
            den *= (points[i].x - points[j].x);
        }

        BigInt term = points[i].y;
        
        term = term.mulInt(num);

        
        long long d = den;
        if (d < 0) {
            term = term.mulInt(-1);
            d = -d;
        }
        term = term.divInt(d);

        ans += term;
    }

    return ans;
}


int main(int argc, char** argv) {
    if (argc < 2) {
        cerr << "Usage: " << argv[0] << " <json_file_1> [<json_file_2> ...]\n";
        return 1;
    }

    for (int fi = 1; fi < argc; ++fi) {
        string path = argv[fi];
        string content = readFile(path);
        TestCase tc = parseTestCase(content);

        if ((int)tc.points.size() < tc.k) {
            cerr << "Error: points provided < k in file " << path << "\n";
            return 1;
        }

        BigInt secret = interpolate_constant_term(tc.points, tc.k);
        cout << "tc" << fi << " secret (c) = " << secret.toString() << "\n";
    }

    return 0;
}
