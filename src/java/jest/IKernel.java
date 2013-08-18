import com.sun.jna.*;
import com.sun.jna.win32.*;
import com.sun.jna.platform.win32.*;

public interface IKernel extends Kernel32 {
    public int GetLastError();
}
