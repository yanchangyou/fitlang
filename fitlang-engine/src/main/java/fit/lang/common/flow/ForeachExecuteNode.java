package fit.lang.common.flow;

import fit.lang.common.AbstractParallelExecuteNode;
import fit.lang.define.base.ExecuteNode;
import fit.lang.define.base.ExecuteNodeData;
import fit.lang.define.base.ExecuteNodeInput;
import fit.lang.define.base.ExecuteNodeOutput;
import fit.lang.aop.ExecuteNodeSimpleAop;
import fit.lang.info.NodeExecuteInfo;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.*;

/**
 * 执行节点
 */
public abstract class ForeachExecuteNode extends AbstractParallelExecuteNode {

    @Override
    public void execute(ExecuteNodeInput input, ExecuteNodeOutput output) {

        ExecutorService executorService = Executors.newFixedThreadPool(parallelism);
        LinkedBlockingDeque<Future<ExecuteNodeData>> resultObjects = new LinkedBlockingDeque<>();

        ExecuteNodeSimpleAop.beforeExecute(input, this, output);

        List<ExecuteNodeData> resultDataList = new ArrayList<>();
        for (int i = 0; next(input); i++) {
            ExecuteNodeOutput subOutput = getCurrentOutput(output);
            ExecuteNodeInput subInput = getCurrentInput(input);

            int index = i;
            Future<ExecuteNodeData> submit = executorService.submit(new Callable<ExecuteNodeData>() {
                @Override
                public ExecuteNodeData call() {
                    input.getNodeContext().setAttribute(getIndexName(), index);
                    ExecuteNodeData result = null;
                    for (ExecuteNode executeNode : childNodes) {
                        executeNode.executeAndNext(subInput, subOutput);
                        result = subOutput.getNodeData();
                        if (isPipe) {
                            subInput.setNodeData(result);
                        }
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

        //total
        NodeExecuteInfo.globalNodeExecuteInfo.setTotal(resultDataList.size());

        ExecuteNodeSimpleAop.beforeExecute(input, this, output);

    }

    public abstract void setForeachOutputList(List<ExecuteNodeData> foreachOutputList, ExecuteNodeOutput output);

    /**
     * 移动指针，返回是有
     *
     * @param input
     * @return
     */
    public abstract boolean next(ExecuteNodeInput input);

    /**
     * 获取当前指针下的元素
     *
     * @param input
     * @return
     */
    public abstract ExecuteNodeInput getCurrentInput(ExecuteNodeInput input);

    public abstract ExecuteNodeOutput getCurrentOutput(ExecuteNodeOutput input);

    /**
     * loop参数处理逻辑，是否pipe模式（出参转下一次入参），默认是否
     */
    protected boolean isPipe;

    protected String indexName = "foreachIndex";

    public boolean isPipe() {
        return isPipe;
    }

    public void setPipe(boolean pipe) {
        isPipe = pipe;
    }

    public void setPipe(Boolean pipe) {
        isPipe = pipe != null ? pipe : false;
    }

    public String getIndexName() {
        return indexName;
    }

    public void setIndexName(String indexName) {
        this.indexName = indexName;
    }
}
