import com.sun.jna.*;
import com.sun.jna.win32.*;
import com.sun.jna.platform.win32.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class CWPStruct extends Structure {
    public WinDef.LPARAM lParam;
    public WinDef.WPARAM wParam;
    public int message;
    public WinDef.HWND handle;

    protected List getFieldOrder() {
	return Arrays.asList(new String[] { "lParam", "wParam", "message", "handle" });
    }
}
