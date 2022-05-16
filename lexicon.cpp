#include "bits.h"//#include <bits/stdc++.h>

using namespace std;

class lexicon{
public:
	lexicon();
	~lexicon();

	void insert(const string &s);
	int lookup(const string &s) const;
	int depth(const string &s) const;
	void replace(const string &s1 , const string &s2);

	friend ostream & operator << (ostream &out , const lexicon &l);
}
