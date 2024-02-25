package fit.lang.plugin.json.ide.node;

import fit.lang.ExecuteNodeException;

/**
 * 用户IDE
 */
public class UserIdeManager {


    static UserIdeInterface userIdeInterface;

    public static UserIdeInterface getUserIdeInterface() {
        if (userIdeInterface == null) {
            throw new ExecuteNodeException("User IDE interface not implement!");
        }
        return userIdeInterface;
    }

    public static void setUserIdeInterface(UserIdeInterface userIdeInterface) {
        UserIdeManager.userIdeInterface = userIdeInterface;
    }

}
