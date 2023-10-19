package fit.lang.common.flow;

public class MyWorker implements Runnable {
    int index;

    public MyWorker(int i) {
        index = i;
    }

    @Override
    public void run() {
        System.out.println(Thread.currentThread().toString() + ":" + index);
    }
}
