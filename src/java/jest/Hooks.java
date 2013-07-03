import com.sun.jna.*;
import com.sun.jna.win32.*;
import com.sun.jna.platform.win32.*;

public interface Hooks {
    public interface IHookProc extends StdCallLibrary.StdCallCallback {
	public WinDef.LRESULT callback(int nCode, WinDef.WPARAM wParam, WinDef.LPARAM lParam);
    }
    public interface ICallWndProc extends StdCallLibrary.StdCallCallback {
	public WinDef.LRESULT callback(int nCode, WinDef.WPARAM wParam, CWPStruct lParam);
    }
    public interface IGetMsgProc extends StdCallLibrary.StdCallCallback {
	public WinDef.LRESULT callback(int nCode, WinDef.WPARAM wParam, WinUser.MSG lParam);
    }
}
