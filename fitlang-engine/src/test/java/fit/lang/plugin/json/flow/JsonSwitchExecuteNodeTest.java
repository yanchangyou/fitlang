package fit.lang.plugin.json.flow;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class JsonSwitchExecuteNodeTest extends TestCase {

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
                "        \"workspaceDir\": \"/opt/github/fitlang/fitlang-server/demo/fitserver/app/postman/\",\n" +
                "        \"info\": {\n" +
                "            \"_postman_id\": \"d6e9d27d-569e-4fca-9a1f-62aa8030a953\",\n" +
                "            \"name\": \"fit-test\",\n" +
                "            \"schema\": \"https://schema.getpostman.com/json/collection/v2.1.0/collection.json\",\n" +
                "            \"_exporter_id\": \"2532575\",\n" +
                "            \"_collection_link\": \"https://gold-star-803610.postman.co/workspace/dev~e159b48c-ff42-45ee-ac1e-014873df4c8a/collection/2532575-d6e9d27d-569e-4fca-9a1f-62aa8030a953?source=collection_link\"\n" +
                "        },\n" +
                "        \"item\": [\n" +
                "            {\n" +
                "                \"name\": \"folder\",\n" +
                "                \"item\": [\n" +
                "                    {\n" +
                "                        \"name\": \"hello\",\n" +
                "                        \"request\": {\n" +
                "                            \"method\": \"POST\",\n" +
                "                            \"header\": [],\n" +
                "                            \"url\": {\n" +
                "                                \"raw\": \"http://fit.321zou.com/hello\",\n" +
                "                                \"protocol\": \"http\",\n" +
                "                                \"host\": [\n" +
                "                                    \"fit\",\n" +
                "                                    \"321zou\",\n" +
                "                                    \"com\"\n" +
                "                                ],\n" +
                "                                \"path\": [\n" +
                "                                    \"hello\"\n" +
                "                                ]\n" +
                "                            }\n" +
                "                        },\n" +
                "                        \"response\": []\n" +
                "                    }\n" +
                "                ]\n" +
                "            },\n" +
                "            {\n" +
                "                \"name\": \"first\",\n" +
                "                \"protocolProfileBehavior\": {\n" +
                "                    \"disableBodyPruning\": true\n" +
                "                },\n" +
                "                \"request\": {\n" +
                "                    \"method\": \"GET\",\n" +
                "                    \"header\": [],\n" +
                "                    \"body\": {\n" +
                "                        \"mode\": \"raw\",\n" +
                "                        \"raw\": \"\",\n" +
                "                        \"options\": {\n" +
                "                            \"raw\": {\n" +
                "                                \"language\": \"json\"\n" +
                "                            }\n" +
                "                        }\n" +
                "                    },\n" +
                "                    \"url\": {\n" +
                "                        \"raw\": \"http://fit.321zou.com/_ip\",\n" +
                "                        \"protocol\": \"http\",\n" +
                "                        \"host\": [\n" +
                "                            \"fit\",\n" +
                "                            \"321zou\",\n" +
                "                            \"com\"\n" +
                "                        ],\n" +
                "                        \"path\": [\n" +
                "                            \"_ip\"\n" +
                "                        ]\n" +
                "                    }\n" +
                "                },\n" +
                "                \"response\": []\n" +
                "            }\n" +
                "        ]\n" +
                "    },\n" +
                "    \"uni\": \"pipe\",\n" +
                "    \"child\": [\n" +
                "//        {\n" +
                "//            \"uni\": \"readFile\",\n" +
                "//            \"filePath\": \"fit-test.postman_collection.json\"\n" +
                "//        },\n" +
                "//        {\n" +
                "//            \"uni\": \"parseJson\",\n" +
                "//            \"jsonField\": \"content\"\n" +
                "//        },\n" +
                "//        {\n" +
                "//            \"uni\": \"replace\",\n" +
                "//            \"json\": {\n" +
                "//                \"item\": \"${content.item}\"\n" +
                "//            }\n" +
                "//        },\n" +
                "        {\n" +
                "            \"uni\": \"foreach\",\n" +
                "            \"foreachField\": \"item\",\n" +
                "            \"child\": [\n" +
                "                {\n" +
                "                    \"id\": \"switch\",\n" +
                "                    \"uni\": \"switch\",\n" +
                "                    \"switchField\": \"${boolean(item)+''}\",\n" +
                "                    \"child\": [\n" +
                "                        {\n" +
                "                            \"id\": \"false\",\n" +
                "                            \"case\": \"false\",\n" +
                "                            \"uni\": \"foreach\",\n" +
                "                            \"foreachField\": \"item\",\n" +
                "                            \"child\": [\n" +
                "                                {\n" +
                "                                    \"uni\": \"mix\",\n" +
                "                                    \"json\": {\n" +
                "                                        \"hello\": \"world\"\n" +
                "                                    }\n" +
                "                                }\n" +
                "                            ]\n" +
                "                        },\n" +
                "                        {\n" +
                "                            \"id\": \"false\",\n" +
                "                            \"case\": \"true\",\n" +
                "                            \"uni\": \"mix\",\n" +
                "                            \"json\": {\n" +
                "                                \"message\": \"here\"\n" +
                "                            }\n" +
                "                        }\n" +
                "                    ]\n" +
                "                }\n" +
                "            ]\n" +
                "        }\n" +
                "        //    ,\n" +
                "        //        {\n" +
                "        //            \"uni\": \"convert\",\n" +
                "        //            \"express\": {\n" +
                "        //                \"filePath\": \"${name}\",\n" +
                "        //                \"content.uni\": \"http\",\n" +
                "        //                \"content.name\": \"${name}\",\n" +
                "        //                \"content.url\": \"${request.url.raw}\",\n" +
                "        //                \"content.input\": \"${request.body.raw}\"\n" +
                "        //            }\n" +
                "        //        },\n" +
                "        //        {\n" +
                "        //            \"uni\": \"mix\",\n" +
                "        //            \"json\": {\n" +
                "        //                \"filePath\": \"${'my/' + filePath+'.fit'}\"\n" +
                "        //            }\n" +
                "        //        },\n" +
                "        //        {\n" +
                "        //            \"uni\": \"writeFile\"\n" +
                "        //        }\n" +
                "    ]\n" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode(flow);

        System.out.println(output);

        JSONObject result = JSONObject.parse(output);
    }
}