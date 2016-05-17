#include "WtfViz2.hpp"
#include <OgreWindowEventUtilities.h>
#include <stdint.h>
#include <map>
#include <string>
#include <vector>

#include "HsFFI.h"

WtfViz2::WtfViz2() {
}

WtfViz2::~WtfViz2() {
    delete OgreFramework::getSingletonPtr();
}

typedef void (*hs_callback_thing)();

void WtfViz2::startViz(HsFunPtr haskell_init, HsFunPtr haskell_update) {
    new OgreFramework();
    if (!OgreFramework::getSingletonPtr()->initOgre("wtf-viz v2.0", this, 0))
        return;

    m_bShutdown = false;

    OgreFramework::getSingletonPtr()->m_pLog->logMessage("wtf-viz initialized!");

    OgreFramework::getSingletonPtr()->m_pLog->logMessage("Calling haskell init...");
    ((hs_callback_thing)haskell_init)();
    OgreFramework::getSingletonPtr()->m_pLog->logMessage("Start main loop...");

    double timeSinceLastFrame = 0;
    double startTime = 0;

    OgreFramework::getSingletonPtr()->m_pRenderWnd->resetStatistics();

    while (!m_bShutdown && !OgreFramework::getSingletonPtr()->isOgreToBeShutDown()) {
        if (OgreFramework::getSingletonPtr()->m_pRenderWnd->isClosed())
            m_bShutdown = true;

        Ogre::WindowEventUtilities::messagePump();

        if (OgreFramework::getSingletonPtr()->m_pRenderWnd->isActive()) {
            startTime = OgreFramework::getSingletonPtr()->m_pTimer->getMillisecondsCPU();

            OgreFramework::getSingletonPtr()->m_pKeyboard->capture();
            OgreFramework::getSingletonPtr()->m_pMouse->capture();

            OgreFramework::getSingletonPtr()->updateOgre(timeSinceLastFrame);
            OgreFramework::getSingletonPtr()->m_pRoot->renderOneFrame();

            // get messages
            ((hs_callback_thing)haskell_update)();

            timeSinceLastFrame =
                OgreFramework::getSingletonPtr()->m_pTimer->getMillisecondsCPU() - startTime;
        } else {
            sleep(1);
        }
    }

    OgreFramework::getSingletonPtr()->m_pLog->logMessage("Main loop quit");
    OgreFramework::getSingletonPtr()->m_pLog->logMessage("Shutdown OGRE...");
}


bool WtfViz2::keyPressed(const OIS::KeyEvent &keyEventRef) {
    OgreFramework::getSingletonPtr()->keyPressed(keyEventRef);

    if (OgreFramework::getSingletonPtr()->m_pKeyboard->isKeyDown(OIS::KC_F)) {
        // do something
    }

    return true;
}

bool WtfViz2::keyReleased(const OIS::KeyEvent &keyEventRef) {
    OgreFramework::getSingletonPtr()->keyReleased(keyEventRef);

    return true;
}
