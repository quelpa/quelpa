# -*- mode: ruby -*-
# vi: set ft=ruby :

vm_ip     = "172.139.149.159"
host_name = "quelpa.lo"

WINDOWS = (RbConfig::CONFIG['host_os'] =~ /mswin|mingw|cygwin/) ? true : false
vagrant_dir = File.dirname(__FILE__) + "/"

Vagrant.configure("2") do |config|

  config.vm.provider :virtualbox do |provider, config|

    config.vm.box = "precise32"
    config.vm.box_url = "http://files.vagrantup.com/precise32.box"
    config.vm.synced_folder ".", "/vagrant", :nfs => !WINDOWS

    provider.customize ["setextradata", :id, "VBoxInternal2/SharedFoldersEnableSymlinksCreate/v-root", "1"]

    config.vm.network :private_network, ip: vm_ip
    config.vm.hostname = host_name

    config.vm.provision "shell", path: "ci/run.sh", privileged: false

  end
end
