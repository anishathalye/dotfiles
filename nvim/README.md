# Neovim 配置

**neovim 版本： 目前仅在0.7.0 版本测试通过， 其余版本请自行测试**

>建议：除了arch系linux，其余linux发行版在[此处](https://github.com/neovim/neovim/releases)下载可执行文件。 通过类似 `apt` 的方式安装往往都是旧版本。

使用方法：

1. clone本仓库，并在 `~/.config` 目录下建立nvim的软链接
  ```shell
  git clone https://github.com/ravenxrz/dotfiles.git ~/.config/dotfiles
  ln -s ~/.config/dotfiles/nvim ~/.config
  ```

2.执行 `nvim +PackerSync` 命令， 根据网络条件的不同，可能需要多次执行本命令，直到所有插件下载完成

3.通过`LspInstallInfo`命令安装对应lsp svr, 接着选中对应语言的server，如cpp为clangd， 按i执行安装

4.通过`DIInstall xxx`命令安装对应语言的 `debugger`, 除此外，需要在 `~/.dotfiles/nvim/lua/user/dap/`目录下新建对应语言的配置文件（已配置好c/cpp,go,python), 同时更新 `dap-config.lua` 文件下的 `config_debuggers()` 函数。

# 额外说明

此仓库bin目录下安装如下可执行文件:
- fd          -- telescope依赖
- glow        -- markdown预览依赖
- lazygit     -- git操作
- rg          -- telescope 依赖


# 交流

**QQ交流群:728502470**


# FAQ

**1.插件下载不下来？**

网络问题，插件都在github上，考虑加代理。

**2.bin目录下的可执行文件如何使用？**

bin目录下的文件是一些依赖可执行文件，如果通过 `install` 命令安装了所有配置，该目录默认会加载至系统PATH变量中，否则需要个人手动将该目录添加至PATH中，另外，这些文件仅在ubuntu20.04中测试使用通过，其余系统不保证。 所以可考虑手动下载依赖, 如 `apt install xxx`, `yum install xx` `pacman -S xxx`等等。

**3.终端颜色有问题?**

> mac建议使用iterm2, 或者kittiy， 不建议使用默认终端。

确认终端支持真彩色, 另外将 $TERM 环境变量设为 `xterm-256color`. 

**4.出现xxx not found**

大概率是因为没有下载完插件。 可以使用 `PackerStatus` 查看下当前有哪些插件。

**5.图标乱码问题**

没有安装nerd font, 请自行安装nerd font，并设置终端字体为对应的nerd font。

**6.PackerSync命令找不到？**

packer插件自动下载失败，网络问题。遇到此问题，可手动清除 `~/.local/share/nvim/site/pack/packer/start/packer.nvim/` 目录，然后重新开启nvim。不过首先要解决的是你的网络问题。

**7. 如何在右上角显示实时cpu和mem？**

这是tmux的配置，和nvim无关，有兴趣可自行查看我的tmux配置。

> 提示：要安装 `ravenxrz/tmux-plugin-sysstat` tmux插件，另外需要手动配置主题。如：
>
> `set -g status-right "#[fg=#1f2428,bg=#1f2428,nobold,nounderscore,noitalics]#[fg=#1f2428,bg=#1f2428] #{prefix_highlight} #[fg=#e1e4e8,bg=#1f2428,nobold,nounderscore,noitalics]#[fg=#586069,bg=#e1e4e8] %Y-%m-%d  %I:%M %p #[fg=#2188ff,bg=#e1e4e8,nobold,nounderscore,noitalics]#[fg=#1f2428,bg=#2188ff,bold] #{sysstat_cpu}#[fg=#1f2428,bg=#2188ff,bold] #{sysstat_mem}#[fg=#2188ff,bg=#2188ff,bold] "`

