package fit.lang.plugin.json.flow;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.aop.ExecuteNodeSimpleAop;
import fit.lang.define.base.*;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.*;

/**
 * 执行节点
 */
public class JsonParallelForeachExecuteNode extends JsonForeachExecuteNode {


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

        ExecutorService executorService = Executors.newFixedThreadPool(parallelism);
        LinkedBlockingDeque<Future<ExecuteNodeData>> resultObjects = new LinkedBlockingDeque<>();

        ExecuteNodeSimpleAop.beforeExecute(input, this, output);

        List<ExecuteNodeData> resultDataList = new ArrayList<>();
        for (int i = 0; next(input); i++) {
            ExecuteNodeOutput subOutput = getCurrentOutput(output);
            ExecuteNodeInput subInput = getCurrentInput(input);

            Future<ExecuteNodeData> submit = executorService.submit(new Callable<ExecuteNodeData>() {
                @Override
                public ExecuteNodeData call() throws Exception {
                    ExecuteNodeData result = null;
                    for (ExecuteNode executeNode : childNodes) {
                        executeNode.executeAndNext(subInput, subOutput);
                        result = subOutput.getNodeData();
                    }
                    return result;
                }
            });
            resultObjects.offer(submit);
        }
        executorService.shutdown();

        resultObjects.forEach(f -> {
            try {
                resultDataList.add(f.get());
            } catch (InterruptedException | ExecutionException e) {
                throw new RuntimeException(e);
            }
        });
        setForeachOutputList(resultDataList, output);

        ExecuteNodeSimpleAop.beforeExecute(input, this, output);

    }

}
