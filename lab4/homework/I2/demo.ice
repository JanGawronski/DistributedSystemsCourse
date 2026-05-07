module Demo {
    struct ItemState {
        int id;
        string name;
        string data;
    };

        interface Base {
        ItemState getState();
        void setState(ItemState s);
        string ping();
    };

    interface Dedicated extends Base {
    };

    interface Shared extends Base {
    };
};
