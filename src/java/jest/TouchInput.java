import com.sun.jna.*;
import com.sun.jna.win32.*;
import com.sun.jna.platform.win32.*;
import java.util.Arrays;
import java.util.List;


public class TouchInput extends Structure {
    public NativeLong x;
    public NativeLong y;
    public WinNT.HANDLE hSource;
    public int dwID;
    public int dwFlags;
    public int dwMask;
    public int dwTime;
    public BaseTSD.ULONG_PTR dwExtraInfo;
    public int cxContact;
    public int cyContact;
    
    protected List getFieldOrder() {
	return Arrays.asList(new String[] { "x", "y", "hSource", "dwID", "dwFlags", "dwMask", "dwTime", "dwExtraInfo", "cxContact", "cyContact" });
    }
}
