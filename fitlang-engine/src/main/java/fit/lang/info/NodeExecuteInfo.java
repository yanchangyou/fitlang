package fit.lang.info;

/**
 *
 */
public class NodeExecuteInfo {

    public static NodeExecuteInfo globalNodeExecuteInfo = new NodeExecuteInfo();

    /**
     * 开始时间：毫秒
     */
    long beginTime;

    /**
     * 结束
     */
    long endTime;

    long costTime;

    /**
     * 总数
     */
    long total;

    /**
     * 性能tps
     */
    long tps;

    public long getBeginTime() {
        return beginTime;
    }

    public void setBeginTime(long beginTime) {
        this.beginTime = beginTime;
    }

    public long getEndTime() {
        return endTime;
    }

    public void setEndTime(long endTime) {
        this.endTime = endTime;
        costTime = endTime - beginTime;
    }

    public long getCostTime() {
        return costTime;
    }

    public void setCostTime(long costTime) {
        this.costTime = costTime;
    }

    public long getTotal() {
        return total;
    }

    public void setTotal(long total) {
        this.total = total;
    }

    public long getTps() {
        return tps;
    }

    public void evalTps(long total) {
        if (costTime != 0) {
            tps = total * 1000 / costTime;
        }
    }
}
