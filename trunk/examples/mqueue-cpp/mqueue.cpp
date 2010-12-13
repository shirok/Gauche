//
// mqueue.c
//
//   The mqueue-cpp extension module to show how to embed external C++
//   library in Gauche.
//   This file is a source of a supposed external C++ library, knowing
//   nothing about Gauche.  In typical case, such external library is
//   provided by the third party.
//

#include "mqueue.h"
#include <functional>
#include <algorithm>

class MQueue_eq : public unary_function<MQueue*, bool> {
    string name;
  public:
    explicit MQueue_eq(const string& name_) : name(name_) {}
    bool operator() (const MQueue* q) const { return q->getName() == name; }
};

set<MQueue*> MQueue::knownQueues;

string MQueue::popMessage() throw (MQueueException)
{
    if (q.empty()) {
        throw MQueueException("attempt to pop from an empty queue");
    }
    Message m = q.top();
    q.pop();
    return m.getBody();
}

size_t MQueue::pushMessage(string body, int urgency)
{
    Message *m = new Message(body, urgency);
    q.push(*m);
    return q.size();
}

void MQueue::registerSelf()
{
    knownQueues.insert(this);
}

void MQueue::unregisterSelf()
{
    knownQueues.erase(this);
}

MQueue *MQueue::findByName(string name)
{
    set<MQueue*>::iterator z = find_if(knownQueues.begin(),
                                       knownQueues.end(),
                                       MQueue_eq(name));
    if (z == knownQueues.end()) return NULL;
    return *z;
}
