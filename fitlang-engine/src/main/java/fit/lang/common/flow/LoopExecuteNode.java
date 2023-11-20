package fit.lang.common.flow;

import fit.lang.common.AbstractParallelExecuteNode;
import fit.lang.define.base.ExecuteNode;
import fit.lang.define.base.ExecuteNodeInput;
import fit.lang.define.base.ExecuteNodeOutput;
import fit.lang.aop.ExecuteNodeSimpleAop;
import fit.lang.info.NodeExecuteInfo;

import java.util.List;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * 执行节点
 */
public abstract class LoopExecuteNode extends AbstractParallelExecuteNode {

    protected int loopTimes = 1;

    transient AtomicInteger currentIndex = new AtomicInteger(0);

    /**
     * loop参数处理逻辑，是否pipe模式（出参转下一次入参），默认是否
     */
    protected boolean isPipe;

    /**
     * 是否袋子模式，是：执行结果放入袋子中，否：最后的结果
     */
    protected boolean isBagsMode;

    protected String bagsName = "list";

    /**
     * 袋子步长：间隔多长放入袋子
     */
    protected int bagsStep = 1;

    /**
     * 获取循环次数
     *
     * @return
     */
    public int getLoopTimes() {
        return loopTimes;
    }

    public void setLoopTimes(Integer loopTimes) {
        if (loopTimes != null) {
            this.loopTimes = loopTimes;
        }
    }

    public void setLoopTimes(int loopTimes) {
        this.loopTimes = loopTimes;
    }

    public int getCurrentIndex() {
        return currentIndex.get();
    }

    public void setCurrentIndex(int currentIndex) {
        this.currentIndex.set(currentIndex);
    }

    public boolean isPipe() {
        return isPipe;
    }

    public void setPipe(boolean pipe) {
        isPipe = pipe;
    }

    public void setPipe(Boolean pipe) {
        isPipe = Boolean.TRUE.equals(pipe);
    }

    public boolean isBagsMode() {
        return isBagsMode;
    }

    public void setBagsMode(boolean bagsMode) {
        this.isBagsMode = bagsMode;
    }

    public String getBagsName() {
        return bagsName;
    }

    public void setBagsName(String bagsName) {
        if (bagsName != null) {
            this.bagsName = bagsName;
        }
    }

    @Override
    public void execute(ExecuteNodeInput input, ExecuteNodeOutput output) {

        ExecutorService executorService = Executors.newFixedThreadPool(parallelism);
        LinkedBlockingDeque<Future<Object>> resultObjects = new LinkedBlockingDeque<>();

        ExecuteNodeSimpleAop.beforeExecute(input, this, output);
        currentIndex.set(0);

        for (int i = 0; i < getLoopTimes(); i++) {

            Future<Object> submit = executorService.submit(new Callable<Object>() {

                @Override
                public Object call() throws Exception {
                    input.getNodeContext().setAttribute("loopIndex", currentIndex.getAndIncrement());
                    for (ExecuteNode executeNode : childNodes) {
                        executeNode.executeAndNext(input, output);
                        if (isPipe) {
                            input.setNodeData(output.getNodeData());
                        }
                    }
                    return output.getNodeData().cloneThis().getData();
                }
            });
            resultObjects.offer(submit);
        }
        executorService.shutdown();

        List<Object> bags = getBags(isBagsMode ? getLoopTimes() : 0);

        AtomicInteger index = new AtomicInteger();
        resultObjects.forEach(f -> {
            try {
                Object object = f.get();

                if (isBagsMode) {
                    if (index.getAndIncrement() % bagsStep == 0) {
                        bags.add(object);
                    }
                }
            } catch (InterruptedException | ExecutionException e) {
                throw new RuntimeException(e);
            }
        });

        if (isBagsMode) {
            setBags(bagsName, bags, output);
        }

        //total
        NodeExecuteInfo.globalNodeExecuteInfo.setTotal(getLoopTimes());

        ExecuteNodeSimpleAop.afterExecute(input, this, output);
    }

    public abstract List getBags(int size);

    public abstract void setBags(String bagsFieldName, List list, ExecuteNodeOutput output);

    public int getBagsStep() {
        return bagsStep;
    }

    public void setBagsStep(int bagsStep) {
        this.bagsStep = bagsStep;
    }
}
