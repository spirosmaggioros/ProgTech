#include <bits/stdc++.h>
using namespace std;

class Graph{
public:
	Graph(int V);
	~Graph();
	void addEdge(int x , int y);
	bool cycle(vector<int> &path) const;
};
