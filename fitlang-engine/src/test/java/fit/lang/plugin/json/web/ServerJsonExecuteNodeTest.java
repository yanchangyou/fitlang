package fit.lang.plugin.json.web;

import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import fit.lang.plugin.json.web.ServerJsonExecuteNode;
import junit.framework.TestCase;

public class ServerJsonExecuteNodeTest extends TestCase {

    public void testExecute() throws InterruptedException {
        ServerJsonExecuteNode.setCurrentServerFilePath("/opt/github/fitlang/fitlang-server/demo/fitserver/app/");
        String flow = "{" +//
                "   'uni': 'server'," +
                "   'port': 11110," +
//                "   'fitPath': '/opt'," +
                "   'fitPath': '/opt/github/fitlang/fitlang-server/demo/fitserver/app/'," +
                "   'service':{" +
                "       '/hello':{" +
                "           'uni':'hello'" +
                "       }," +
                "       '/execute': {" +
                "            'uni': 'execute'" +
                "        }" +
                "   }" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        System.out.println(output);
//        Thread.sleep(1000 * 1000);
    }


    public void testExecute1() throws InterruptedException {
        String flow = "{\n" +
                "    \"uni\": \"loop\",\n" +
                "    \"loopTimes\": 1,\n" +
                "    \"isPipe\": true,\n" +
                "    \"isBagsMode\": true,\n" +
                "    \"bagsName\": 'list',\n" +
                "    \"child\": [\n" +
                "        {\n" +
                "            \"uni\": \"mix\",\n" +
                "            \"json\": {\n" +
                "                \"port\": \"${11110+loopIndex}\"\n" +
                "            }\n" +
                "        },\n" +
                "        {\n" +
                "            \"uni\": \"server\",\n" +
                "            \"welcome\": \"welcome to fit lang world!\",\n" +
                "            \"action\": {\n" +
                "                \"/hello\": {\n" +
                "                    \"uni\": \"hello\"\n" +
                "                }\n" +
                "            }\n" +
                "        }\n" +
                "    ]\n" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        System.out.println(output);
//        Thread.sleep(1000 * 1000);
    }

}