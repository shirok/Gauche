// -*- mode: C++ -*-
//
// mqueue.h - a simple-mineded message queue with priority
//
//   The mqueue-cpp extension module to show how to embed external C++
//   library in Gauche.
//   This file is a header of a supposed external C++ library, knowing
//   nothing about Gauche.  In typical case, such external library is
//   provided by the third party.
//

#ifndef MQUEUE_H
#define MQUEUE_H

#include <queue>
#include <string>
#include <set>

using namespace std;

class Message {
  public:
    Message(string body_, int urgency_ = 0)
        : body(body_), urgency(urgency_)
        {}

    bool operator< (const Message& m) const {
        return urgency < m.urgency;
    }

    int getUrgency() const { return urgency;}
    string getBody() const { return body; }

  private:
    string body;
    int urgency;
};

class MQueueException {
  public:
    string reason;
    MQueueException(string reason_) : reason(reason_) {}
};

class MQueue {
  public:
    MQueue(string name_) : name(name_) { registerSelf(); }
    ~MQueue() { unregisterSelf(); }

    string getName() const { return name; }

    // Basic queue operations.  The client doesn't need to know
    // about Message.
    bool empty() const      { return q.empty(); }
    string popMessage() throw (MQueueException);
    size_t pushMessage(string body, int urgency = 0);

    // One can find a previously created MQueue by its name.
    static MQueue *findByName(string name);

    bool operator< (const MQueue& m) const { return name < m.name; }

  private:
    string name;
    priority_queue<Message> q;

    static set<MQueue*> knownQueues;
    void registerSelf();
    void unregisterSelf();
};

    
#endif  // MQUEUE_H

