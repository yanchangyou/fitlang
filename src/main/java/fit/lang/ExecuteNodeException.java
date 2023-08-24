package fit.lang;

public class ExecuteNodeException extends RuntimeException {

    public ExecuteNodeException(String message) {
        super(message);
    }

    public ExecuteNodeException(String message, Exception exception) {
        super(message, exception);
    }
}
