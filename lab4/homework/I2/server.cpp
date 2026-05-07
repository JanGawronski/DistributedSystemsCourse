#include <Ice/Ice.h>
#include "demo.h"
#include <iostream>
#include <unordered_map>
#include <list>
#include <mutex>
#include <fstream>

using namespace std;

class DedicatedServant : public Demo::Dedicated {
public:
    DedicatedServant(const Ice::Identity& id) : _id(id) {
        try { _state.id = std::stoi(id.name); } catch(...) { _state.id = 0; }
        _state.name = string("obj-") + id.name;
        _state.data = "";
        cout << "[Server] DedicatedServant created for id=" << id.name << "\n";
    }

    Demo::ItemState getState(const Ice::Current&) override {
        return _state;
    }

    void setState(const Demo::ItemState& s, const Ice::Current&) override {
        _state = s;
    }

    string ping(const Ice::Current&) override {
        return string("pong-") + _id.name;
    }

    void saveToFile(const string& path) {
        ofstream f(path);
        if(!f) return;
        f << _state.id << "\n" << _state.name << "\n" << _state.data << "\n";
    }

    void loadFromFile(const string& path) {
        ifstream f(path);
        if(!f) return;
        string line;
        if(!getline(f, line)) return;
        _state.id = stoi(line);
        getline(f, _state.name);
        getline(f, _state.data);
    }

private:
    Ice::Identity _id;
    Demo::ItemState _state;
};

class SharedServant : public Demo::Shared {
public:
    SharedServant() { cout << "[Server] SharedServant instantiated\n"; }
    Demo::ItemState getState(const Ice::Current&) override { return _state; }
    void setState(const Demo::ItemState& s, const Ice::Current&) override { _state = s; }
    string ping(const Ice::Current&) override { return string("shared-pong"); }
private:
    Demo::ItemState _state;
};

class MyServantLocator : public Ice::ServantLocator {
public:
    MyServantLocator(size_t maxServants=5): _max(maxServants) {}

    Ice::ObjectPtr locate(const Ice::Current& curr, Ice::LocalObjectPtr& cookie) override {
        lock_guard<mutex> g(_m);
        string name = curr.id.name;
        if(_map.count(name)) {
            touch(name);
            cout << "[Server] Reusing DedicatedServant for id=" << name << "\n";
            return _map[name];
        }
        Ice::Identity id = curr.id;
        auto ds = new DedicatedServant(id);
        string fname = string("state_") + name + ".txt";
        ds->loadFromFile(fname);
        Ice::ObjectPtr obj = ds;
        _map[name] = obj;
        _lru.push_front(name);
        _iter[name] = _lru.begin();
        cout << "[Server] Instantiated DedicatedServant for id=" << name << "\n";
        if(_map.size() > _max) {
            string evict = _lru.back();
            cout << "[Server] Evicting servant id=" << evict << "\n";
            auto eds = dynamic_cast<DedicatedServant*>(_map[evict].get());
            if(eds) eds->saveToFile(string("state_") + evict + ".txt");
            _map.erase(evict);
            _iter.erase(evict);
            _lru.pop_back();
        }
        return obj;
    }

    void finished(const Ice::Current& curr, const Ice::ObjectPtr& servant, const Ice::LocalObjectPtr& cookie) override {
        lock_guard<mutex> g(_m);
        string name = curr.id.name;
        if(_map.count(name)) touch(name);
    }

    void deactivate(const ::std::string& category) override {
    }

private:
    void touch(const string& name) {
        if(_iter.count(name)) {
            _lru.erase(_iter[name]);
        }
        _lru.push_front(name);
        _iter[name] = _lru.begin();
    }

    unordered_map<string, Ice::ObjectPtr> _map;
    list<string> _lru;
    unordered_map<string, list<string>::iterator> _iter;
    mutex _m;
    size_t _max;
};

int main(int argc, char* argv[]) {
    try {
        Ice::CommunicatorHolder ich(argc, argv);
        auto communicator = ich.communicator();

        Ice::ObjectAdapterPtr adapter = communicator->createObjectAdapterWithEndpoints("DemoAdapter", "default -p 10000");

        Ice::ObjectPtr shared = new SharedServant();
        adapter->addDefaultServant(shared, "shared");

        Ice::ServantLocatorPtr locator = new MyServantLocator(3);
        adapter->addServantLocator(locator, "dedicated");

        adapter->activate();

        cout << "[Server] Running. Adapter activated.\n";
        communicator->waitForShutdown();

    } catch(const exception& ex) {
        cerr << "Exception: " << ex.what() << "\n";
        return 1;
    }
    return 0;
}
