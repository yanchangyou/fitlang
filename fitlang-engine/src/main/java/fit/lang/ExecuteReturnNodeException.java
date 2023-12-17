package fit.lang;

import com.alibaba.fastjson2.JSONObject;

/**
 * return节点，异常实现
 */
public class ExecuteReturnNodeException extends RuntimeException {

    JSONObject result;

    public ExecuteReturnNodeException(JSONObject result) {
        this.result = result;
    }

    public JSONObject getResult() {
        return result;
    }
}
