package fit.lang.plugin.json.flow;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import fit.lang.plugin.json.JsonDynamicFlowExecuteEngine;
import junit.framework.TestCase;
import org.junit.Assert;

public class JsonSwitchExecuteNodeTest extends TestCase {

    @Override
    protected void setUp() throws Exception {
        JsonDynamicFlowExecuteEngine.enableUnsafeNodes();
    }

    public void testExecuteCase1() {

        testExecute("1", "message", "hello, world!");

    }

    public void testExecuteCase2() {

        testExecute("2", "type", "2");

    }

    public void testExecute(String type, String outputFieldName, String expect) {
        String flow = "{" +//
                "   'uni': 'switch'," +
                "   'switchField': 'type'," +
                "   'child': [" +
                "       {" +
                "           'case':'1'," +
                "           'uni':'hello'" +
                "       }," +
                "       {" +
                "           'case':'2'," +
                "           'uni':'echo'" +
                "       }" +
                "   ]" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{'type':'" + type + "'}", flow);

        System.out.println(output);

        JSONObject result = JSONObject.parse(output);
        Assert.assertEquals(expect, result.get(outputFieldName));
    }

    public void testExecute2() {
        String flow = "{\n" +
                "    \"flag\": \"_needFormatJsonInConsole\",\n" +
                "    \"input\": {\n" +
                "        \"workspaceDir\": \"/opt/github/fitlang/fitlang-server/demo/fitserver/app/postman/\"\n" +
                "    },\n" +
                "    \"uni\": \"pipe\",\n" +
                "    \"child\": [\n" +
                "        {\n" +
                "            \"uni\": \"readFile\",\n" +
                "            \"filePath\": \"fit-test.postman_collection.json\"\n" +
                "        },\n" +
                "        {\n" +
                "            \"uni\": \"parseJson\",\n" +
                "            \"jsonField\": \"content\"\n" +
                "        },\n" +
                "        {\n" +
                "            \"uni\": \"replace\",\n" +
                "            \"json\": {\n" +
                "                \"item\": \"${content.item}\",\n" +
                "                \"collectionName\": \"${content.info.name}\"\n" +
                "            }\n" +
                "        },\n" +
                "        {\n" +
                "            \"uni\": \"set\",\n" +
                "            \"key\": \"collectionName\"\n" +
                "        },\n" +
                "        {\n" +
                "            \"uni\": \"foreach\",\n" +
                "            \"foreachField\": \"item\",\n" +
                "            \"child\": {\n" +
                "                \"uni\": \"pipe\",\n" +
                "                \"child\": [\n" +
                "                    {\n" +
                "                        \"uni\": \"mix\",\n" +
                "                        \"json\": {\n" +
                "                            \"parentName\": \"${name}\"\n" +
                "                        }\n" +
                "                    },\n" +
                "                    {\n" +
                "                        \"id\": \"switch\",\n" +
                "                        \"uni\": \"switch\",\n" +
                "                        \"switchField\": \"${boolean(item)+''}\",\n" +
                "                        \"child\": [\n" +
                "                            {\n" +
                "                                \"id\": \"true\",\n" +
                "                                \"case\": \"true\",\n" +
                "                                \"uni\": \"foreach\",\n" +
                "                                \"foreachField\": \"item\",\n" +
                "                                \"mixToItemField\": \"parentName\",\n" +
                "                                \"child\": [\n" +
                "                                    {\n" +
                "                                        \"id\": \"false\",\n" +
                "                                        \"case\": \"false\",\n" +
                "                                        \"uni\": \"pipe\",\n" +
                "                                        \"child\": [\n" +
                "                                            {\n" +
                "                                                \"uni\": \"convert\",\n" +
                "                                                \"express\": {\n" +
                "                                                    \"parentName\": \"${parentName}\",\n" +
                "                                                    \"name\": \"${name}\",\n" +
                "                                                    \"content.uni\": \"http\",\n" +
                "                                                    \"content.name\": \"${name}\",\n" +
                "                                                    \"content.url\": \"${request.url.raw}\",\n" +
                "                                                    \"content.input\": \"${request.body.raw}\"\n" +
                "                                                }\n" +
                "                                            },\n" +
                "                                            {\n" +
                "                                                \"uni\": \"mix\",\n" +
                "                                                \"json\": {\n" +
                "                                                    \"filePath\": \"${collectionName + '/' + parentName + '/' + name + '.fit'}\"\n" +
                "                                                }\n" +
                "                                            },\n" +
                "                                            {\n" +
                "                                                \"uni\": \"writeFile\",\n" +
                "                                                \"format\": true\n" +
                "                                            }\n" +
                "                                        ]\n" +
                "                                    }\n" +
                "                                ]\n" +
                "                            },\n" +
                "                            {\n" +
                "                                \"id\": \"false\",\n" +
                "                                \"case\": \"false\",\n" +
                "                                \"uni\": \"pipe\",\n" +
                "                                \"child\": [\n" +
                "                                    {\n" +
                "                                        \"uni\": \"convert\",\n" +
                "                                        \"express\": {\n" +
                "                                            \"name\": \"${name}\",\n" +
                "                                            \"content.uni\": \"http\",\n" +
                "                                            \"content.name\": \"${name}\",\n" +
                "                                            \"content.url\": \"${request.url.raw}\",\n" +
                "                                            \"content.input\": \"${request.body.raw}\"\n" +
                "                                        }\n" +
                "                                    },\n" +
                "                                    {\n" +
                "                                        \"uni\": \"mix\",\n" +
                "                                        \"json\": {\n" +
                "                                            \"collectionName\":\"${collectionName}\",\n" +
                "                                            \"filePath\": \"${collectionName + '/' + name+'.fit'}\"\n" +
                "                                        }\n" +
                "                                    },\n" +
                "//                                    {\n" +
                "//                                        \"uni\": \"writeFile\",\n" +
                "//                                        \"format\": true\n" +
                "//                                    }\n" +
                "                                ]\n" +
                "                            }\n" +
                "                        ]\n" +
                "                    }\n" +
                "                ]\n" +
                "            }\n" +
                "        }\n" +
                "    ]\n" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode(flow);

        System.out.println(output);

        JSONObject result = JSONObject.parse(output);
    }
}