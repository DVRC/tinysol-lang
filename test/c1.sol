//SPDX-License-Identifier: GPL-3.0-only

contract C1 {
    int x;
    bool b;
  
    function f1() public { 
        if (b) x = x+1;
        else b=true;
    }

    function f2(address a) public {
        a.transfer(1);
    }

    function f3(int amt) public {
        if (this.balance < 8) b=false;
        else msg.sender.transfer(amt);
    }

}
