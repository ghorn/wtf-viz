#pragma once

#include <map>
#include <string>
#include <vector>

#include "OgreFramework.hpp"

#include "HsFFI.h"

class WtfViz2 : public OIS::KeyListener {
 public:
    WtfViz2();
    ~WtfViz2();

    void startViz(HsFunPtr haskell_init, HsFunPtr haskell_update);

    bool keyPressed(const OIS::KeyEvent &keyEventRef);
    bool keyReleased(const OIS::KeyEvent &keyEventRef);

 private:
    bool m_bShutdown;
};
