#ifndef CRITERIUM_MESSAGE_QUEUE_H
#define CRITERIUM_MESSAGE_QUEUE_H

#include <condition_variable>
#include <mutex>
#include <queue>

namespace criterium {

// Thread-safe blocking message queue.
// - push() adds a message and notifies one waiting consumer
// - pop() blocks until a message is available or stop() is called
// - stop() signals consumers to exit; pop() returns false when stopped and empty
template<typename T>
class MessageQueue {
  std::queue<T> queue;
  std::mutex mutex;
  std::condition_variable cond;
  bool stopped = false;

public:
  void push(T msg) {
    std::lock_guard<std::mutex> lock(mutex);
    queue.push(std::move(msg));
    cond.notify_one();
  }

  bool pop(T& msg) {
    std::unique_lock<std::mutex> lock(mutex);
    while (queue.empty() && !stopped) {
      cond.wait(lock);
    }
    if (stopped && queue.empty()) {
      return false;
    }
    msg = std::move(queue.front());
    queue.pop();
    return true;
  }

  void stop() {
    std::lock_guard<std::mutex> lock(mutex);
    stopped = true;
    cond.notify_all();
  }
};

} // namespace criterium

#endif // CRITERIUM_MESSAGE_QUEUE_H
