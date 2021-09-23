# 打印结构体
set print pretty on

# macos 防卡住
set startup-with-shell off

# 保存gdb历史命令
set history save on

# 退出gdb无提示
define hook-quit
    set confirm off
end
