package fit.lang.plugin.json.ide.jcef;

import com.alibaba.fastjson2.JSONArray;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import fit.lang.plugin.json.ExpressUtil;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import java.util.ArrayList;
import java.util.List;

/**
 * 执行节点
 */
public class JcefOpenJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {
        JSONArray urlArray = input.getData().getJSONArray("urls");
        String url = ExecuteJsonNodeUtil.parseStringField("url", input, nodeJsonDefine);
        if (urlArray != null) {
            List<String> list = new ArrayList<>(urlArray.size());
            for (int i = 0; i < urlArray.size(); i++) {
                String item = urlArray.getString(i);
                item = (String) ExpressUtil.eval(item, input.getInputParamAndContextParam());
                list.add(item);
            }
            String[] urls = list.toArray(new String[0]);
            FitJcefManager.open(urls);
            output.set("urls", urls);
        }
        if (url != null) {
            FitJcefManager.open(url);
            output.set("url", url);
        }
    }
}
