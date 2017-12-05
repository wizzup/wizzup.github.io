หากต้องการแค่ใช้ code ล่าสุดโดยไม่สนใจประวัติการแก้ไขก่อนหน้านั้น หรือต้องการใช้กับ Continuous Integration (CI) เราสามาถเลือกความลึกของประวัิตได้ ด้วย `--depth n`

**ตัวอย่าง**: ความลึกเท่ากับ 1 จะได้ commit ล่าสุดเพียง commit เดียว ด้วยขนาด 14MiB

    $ git clone --depth 1 https://github.com/NixOS/nixpkgs
    Cloning into 'nixpkgs'...
    remote: Counting objects: 20622, done.
    remote: Compressing objects: 100% (14606/14606), done.
    remote: Total 20622 (delta 592), reused 14395 (delta 339), pack-reused 0
    Receiving objects: 100% (20622/20622), 14.38 MiB | 152.00 KiB/s, done.
    Resolving deltas: 100% (592/592), done.

    $ git log --oneline
    87b215d5 (grafted, HEAD -> master, origin/master, origin/HEAD) android-studio-preview: 3.0.0.7 -> 3.0.0.8

เทียบกับ full clone: ได้ประวัติตั้งแต่เริ่ม ด้วยขนาด 513Mib

    $ git clone https://github.com/NixOS/nixpkgs
    Cloning into 'nixpkgs'...
    remote: Counting objects: 959262, done.
    remote: Compressing objects: 100% (15/15), done.
    remote: Total 959262 (delta 5), reused 3 (delta 3), pack-reused 959244
    Receiving objects: 100% (959262/959262), 513.09 MiB | 35.00 KiB/s, done.
    Resolving deltas: 100% (641363/641363), done.

หรือถ้าต้องการแค่ไฟล์ ไม่รวมประวัติของ git ก็สามารถดาวน์โหลด ได้เลยโดยตรงผ่าน url

* master branch @ HEAD

    https://github.com/NixOS/nixpkgs/archive/master.zip
    https://github.com/NixOS/nixpkgs/archive/master.tar.gz

* commit 70f35a99a4f7d17f8b82105e648efb3de681ab8e

    https://github.com/NixOS/nixpkgs/archive/70f35a99a4f7d17f8b82105e648efb3de681ab8e.zip
    https://github.com/NixOS/nixpkgs/archive/70f35a99a4f7d17f8b82105e648efb3de681ab8e.tar.gz
