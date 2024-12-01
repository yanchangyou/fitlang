package fit.lang.plugin.json.expression;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.define.ExecuteNodeBuildable;

public interface ExecuteNodeEval extends ExecuteNodeBuildable {

    JSONObject eval(JSONObject express);

}
