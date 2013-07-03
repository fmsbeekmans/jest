import com.sun.jna.*;
import com.sun.jna.win32.*;
import com.sun.jna.platform.win32.*;

public interface IUser extends WinUser {
    public int GetSystemMetrics(int kind);
    public boolean RegisterTouchWindow(WinDef.HWND handle, int flags);
    public BaseTSD.LONG_PTR SendMessageA(WinDef.HWND handle, int messageType, WinDef.WPARAM wparam, WinDef.LPARAM lparam);
    public BaseTSD.LONG_PTR SendMessageA(WinDef.HWND handle, int messageType, long wparam, Pointer lparam);
    public BaseTSD.LONG_PTR SendMessageA(WinDef.HWND handle, int messageType, IAWTCallback callback, WinDef.LPARAM lparam);
    public WinDef.HWND FindWindowA(String classname, String name);
    public WinDef.DWORD GetWindowThreadProcessId(WinDef.HWND handle, Pointer processOut);
    public WinUser.HHOOK SetWindowsHookExA (int idHook, WinUser.HOOKPROC lpfn, WinDef.HINSTANCE hMod, WinDef.DWORD dwThreadId);
    public WinUser.HHOOK SetWindowsHookExA (int idHook, Hooks.IHookProc lpfn, WinDef.HINSTANCE hMod, WinDef.DWORD dwThreadId);
    public WinUser.HHOOK SetWindowsHookExA (int idHook, Hooks.ICallWndProc lpfn, WinDef.HINSTANCE hMod, WinDef.DWORD dwThreadId);
    public WinUser.HHOOK SetWindowsHookExA (int idHook, Hooks.IGetMsgProc lpfn, WinDef.HINSTANCE hMod, WinDef.DWORD dwThreadId);
    public boolean UnhookWindowsHookEx (WinUser.HHOOK hhk);
    public WinDef.LRESULT CallNextHookEx (WinUser.HHOOK hook, int ncode, WinDef.WPARAM wParam, WinDef.LPARAM lParam);
    public WinDef.LRESULT CallNextHookEx (WinUser.HHOOK hook, int ncode, WinDef.WPARAM wParam, WinUser.MSG lParam);
    public WinDef.LRESULT CallNextHookEx (WinUser.HHOOK hook, int ncode, WinDef.WPARAM wParam, CWPStruct lParam);
    public boolean GetTouchInputInfo (WinNT.HANDLE touchinput, int count, TouchInput[] inputArray, int inputSize);
}
