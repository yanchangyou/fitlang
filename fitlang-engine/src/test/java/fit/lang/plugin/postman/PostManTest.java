package fit.lang.plugin.postman;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import fit.lang.plugin.json.JsonDynamicFlowExecuteEngine;
import junit.framework.TestCase;
import org.junit.Assert;

public class PostManTest extends TestCase {

    @Override
    protected void setUp() throws Exception {
        JsonDynamicFlowExecuteEngine.enableUnsafeNodes();
    }

    public void testExecute() {
        String flow = "{\n" +
                "    \"input\": {\n" +
                "        \"workspaceDir\": \"/opt/github/fitlang/fitlang-server/demo/fitserver/app/postman/\",\n" +
                "        \"filePath\": \"fit-test.postman_collection.json\"\n" +
                "    },\n" +
                "    \"uni\": \"pipe\",\n" +
                "    \"child\": [\n" +
                "        {\n" +
                "            \"uni\": \"readFile\"\n" +
                "        },\n" +
                "        {\n" +
                "            \"uni\": \"parseJson\",\n" +
                "            \"jsonField\": \"content\"\n" +
                "        },\n" +
                "        {\n" +
                "            \"uni\": \"convert\",\n" +
                "            \"express\": {\n" +
                "                \"item\": \"${content.item}\",\n" +
                "                \"collectionName\": \"${content.info.name}\"\n" +
                "            }\n" +
                "        },\n" +
                "        {\n" +
                "            \"uni\": \"setGlobal\",\n" +
                "            \"key\": \"collectionName\",\n" +
                "            \"value\": \"${collectionName}\"\n" +
                "        },\n" +
                "        {\n" +
                "            \"uni\": \"foreach\",\n" +
                "            \"foreachField\": \"item\",\n" +
                "            \"child\": {\n" +
                "                \"id\": \"switch\",\n" +
                "                \"uni\": \"switch\",\n" +
                "                \"switchField\": \"${this.?item!=null}\",\n" +
                "                \"child\": [\n" +
                "                    {\n" +
                "                        \"case\": \"false\",\n" +
                "                        \"uni\": \"pipe\",\n" +
                "                        \"child\": [\n" +
                "                            {\n" +
                "                                \"uni\": \"mix\",\n" +
                "                                \"json\": {\n" +
                "                                    \"isFolder\": false\n" +
                "                                }\n" +
                "                            },\n" +
                "                            {\n" +
                "                                \"id\": \"convertItem\",\n" +
                "                                \"uni\": \"pipe\",\n" +
                "                                \"child\": [\n" +
                "                                    {\n" +
                "                                        \"uni\": \"convert\",\n" +
                "                                        \"express\": {\n" +
                "                                            \"name\": \"${name}\",\n" +
                "                                            \"content.uni\": \"http\",\n" +
                "                                            \"content.name\": \"${name}\",\n" +
                "                                            \"content.url\": \"${request.url.raw}\",\n" +
                "                                            \"content.param\": \"${request.body.raw}\",\n" +
                "                                            \"isFolder\": \"${isFolder}\"\n" +
                "                                        }\n" +
                "                                    },\n" +
                "                                    {\n" +
                "                                        \"uni\": \"mix\",\n" +
                "                                        \"json\": {\n" +
                "                                            \"filePath\": \"${collectionName + (isFolder?parentName:'') + '/' + name+'.fit'}\"\n" +
                "                                        }\n" +
                "                                    },\n" +
                "                                    {\n" +
                "                                        \"uni\": \"parseJson\",\n" +
                "                                        \"jsonField\": \"content.param\"\n" +
                "                                    },\n" +
                "                                    {\n" +
                "                                        \"uni\": \"writeFile\",\n" +
                "                                        \"format\": true\n" +
                "                                    }\n" +
                "                                ]\n" +
                "                            }\n" +
                "                        ]\n" +
                "                    },\n" +
                "                    {\n" +
                "                        \"case\": \"true\",\n" +
                "                        \"uni\": \"pipe\",\n" +
                "                        \"child\": [\n" +
                "                            {\n" +
                "                                \"uni\": \"mix\",\n" +
                "                                \"json\": {\n" +
                "                                    \"parentName\": \"${'/'+name}\",\n" +
                "                                    \"isFolder\": true\n" +
                "                                }\n" +
                "                            },\n" +
                "                            {\n" +
                "                                \"uni\": \"foreach\",\n" +
                "                                \"foreachField\": \"item\",\n" +
                "                                \"mixToItemField\": [\n" +
                "                                    \"parentName\",\n" +
                "                                    \"isFolder\"\n" +
                "                                ],\n" +
                "                                \"child\": {\n" +
                "                                    \"uni\": \"call\",\n" +
                "                                    \"nodeId\": \"convertItem\"\n" +
                "                                }\n" +
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