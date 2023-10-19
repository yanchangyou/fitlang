package fit.lang.plugin.json.flow;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.aop.ExecuteNodeSimpleAop;
import fit.lang.define.base.*;
import org.apache.commons.collections.list.SynchronizedList;

import java.util.List;
import java.util.concurrent.*;

/**
 * 执行节点
 */
public class JsonParallelLoopExecuteNode extends JsonLoopExecuteNode {

    protected int parallelism = Runtime.getRuntime().availableProcessors();

    public int getParallelism() {
        return parallelism;
    }

    public void setParallelism(int parallelism) {
        this.parallelism = parallelism;
    }

    public void setParallelism(Integer parallelism) {
        if (parallelism != null) {
            this.parallelism = parallelism;
        }
    }

    /**
     * 静态解析
     *
     * @param executeNodeData
     */
    @Override
    public void build(ExecuteNodeData executeNodeData) {

        JSONObject nodeDefineJson = (JSONObject) nodeDefine.getData();

        setParallelism(nodeDefineJson.getInteger("parallelism"));

        super.build(executeNodeData);

    }

    @Override
    public void execute(ExecuteNodeInput input, ExecuteNodeOutput output) {

        ExecuteNodeSimpleAop.beforeExecute(input, this, output);
        currentIndex = 0;
        List bags = SynchronizedList.decorate(getBags(getLoopTimes()));

        ExecutorService executorService = Executors.newFixedThreadPool(parallelism);
        LinkedBlockingDeque<Future<Object>> resultObjects = new LinkedBlockingDeque<>();

        for (int i = 0; i < getLoopTimes(); i++) {
            input.getNodeContext().setAttribute("loopIndex", currentIndex);
            Future<Object> submit = executorService.submit(new Callable<Object>() {

                @Override
                public Object call() throws Exception {
                    for (ExecuteNode executeNode : childNodes) {
                        executeNode.executeAndNext(input, output);
                        if (isPipe) {
                            input.setNodeData(output.getNodeData());
                        }
                    }
                    if (isBagsMode) {
                        bags.add(output.getNodeData().getData());
                    }

                    return new Object();
                }
            });
            resultObjects.offer(submit);
            currentIndex++;
        }
//        executorService.shutdown();

        resultObjects.forEach(f -> {
            try {
                if (f != null) {
                    f.get();
                }
            } catch (InterruptedException | ExecutionException e) {
                throw new RuntimeException(e);
            }
        });

        if (isBagsMode) {
            setBags(bagsName, bags, output);
        }

        ExecuteNodeSimpleAop.afterExecute(input, this, output);
    }
}
