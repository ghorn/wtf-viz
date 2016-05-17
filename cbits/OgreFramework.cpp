#include <sstream>
#include <string>
#include <algorithm>

#include "OgreFramework.hpp"

#include <OgreTextureManager.h>
#include "Overlay/OgreOverlaySystem.h"

// #define FONT_FOLDER "/usr/share/fonts/truetype/freefont"
// #define FONT_FILE_NAME "FreeMono.ttf"

#define FONT_FOLDER "/usr/share/fonts/truetype/ttf-dejavu"
#define FONT_FILE_NAME "DejaVuSansMono.ttf"


template<> OgreFramework* Ogre::Singleton<OgreFramework>::msSingleton = 0;

OgreFramework::OgreFramework() {
    m_MoveSpeed       = 0.1f;
    m_RotateSpeed     = 0.3f;

    m_bShutDownOgre   = false;
    m_iNumScreenShots = 0;

    m_pRoot           = 0;
    m_pSceneMgr       = 0;
    m_pRenderWnd      = 0;
    m_pCamera         = 0;
    m_pViewport       = 0;
    m_pLog            = 0;
    m_pTimer          = 0;

    m_pInputMgr       = 0;
    m_pKeyboard       = 0;
    m_pMouse          = 0;

    m_focusSphere = 0;
    m_focusSphereNode = 0;

    //  m_pTrayMgr        = 0;
    m_FrameEvent      = Ogre::FrameEvent();

    m_debugOverlay = 0;

    m_cameraFocus = Ogre::Vector3::ZERO;
    m_cameraAzimuth = 0;
    m_cameraElevation = 0;
    m_cameraRadius = 100;
}

bool OgreFramework::initOgre(Ogre::String wndTitle,
                             OIS::KeyListener *pKeyListener,
                             OIS::MouseListener *pMouseListener) {
    //  Ogre::LogManager* logMgr = new Ogre::LogManager();
    new Ogre::LogManager();
    m_pLog = Ogre::LogManager::getSingleton().createLog("OgreLogfile.log", true, true, false);
    m_pLog->setDebugOutputEnabled(true);

    m_pRoot = new Ogre::Root();

    if (!(m_pRoot->restoreConfig() || m_pRoot->showConfigDialog()))
        return false;
    m_pRenderWnd = m_pRoot->initialise(true, wndTitle);

    // Make the scene manager.
    m_pSceneMgr = m_pRoot->createSceneManager(Ogre::ST_GENERIC, "SceneManager");
    // Make overlay system and add to scene maneger.
    Ogre::OverlaySystem *mOverlaySystem = OGRE_NEW Ogre::OverlaySystem();
    m_pSceneMgr->addRenderQueueListener(mOverlaySystem);

    // Set some ambient light
    m_pSceneMgr->setAmbientLight(Ogre::ColourValue(0.7f, 0.7f, 0.7f));

    // Initialize the camera.
    m_pCamera = m_pSceneMgr->createCamera("Camera");
    m_pCamera->setPosition(Ogre::Vector3(0, 60, 60));
    m_pCamera->lookAt(Ogre::Vector3(0, 0, 0));
    m_pCamera->setNearClipDistance(1);
    m_pCamera->setAutoAspectRatio(true);

    // Initialize the viewport
    m_pViewport = m_pRenderWnd->addViewport(m_pCamera);
    m_pViewport->setBackgroundColour(Ogre::ColourValue(0.8f, 0.7f, 0.6f, 1.0f));
    m_pViewport->setCamera(m_pCamera);

    // Make it so that ogre doesn't steal mouse/keyboard.
    size_t hWnd = 0;
    OIS::ParamList paramList;
    m_pRenderWnd->getCustomAttribute("WINDOW", &hWnd);
    paramList.insert(OIS::ParamList::value_type("WINDOW", Ogre::StringConverter::toString(hWnd)));
    paramList.insert(OIS::ParamList::value_type("x11_mouse_hide", "false"));
    paramList.insert(OIS::ParamList::value_type("x11_mouse_grab", "false"));
    paramList.insert(OIS::ParamList::value_type("x11_keyboard_grab", "false"));

    // Set up the input manager.
    m_pInputMgr = OIS::InputManager::createInputSystem(paramList);

    m_pKeyboard =
        static_cast<OIS::Keyboard*>(m_pInputMgr->createInputObject(OIS::OISKeyboard, true));
    m_pMouse =
        static_cast<OIS::Mouse*>(m_pInputMgr->createInputObject(OIS::OISMouse, true));

    m_pMouse->getMouseState().height = m_pRenderWnd->getHeight();
    m_pMouse->getMouseState().width  = m_pRenderWnd->getWidth();

    if (pKeyListener == 0)
        m_pKeyboard->setEventCallback(this);
    else
        m_pKeyboard->setEventCallback(pKeyListener);

    if (pMouseListener == 0)
        m_pMouse->setEventCallback(this);
    else
        m_pMouse->setEventCallback(pMouseListener);

    // Load resources
    Ogre::String secName, typeName, archName;
    Ogre::ConfigFile cf;
    cf.load("resources.cfg");

    Ogre::ConfigFile::SectionIterator seci = cf.getSectionIterator();
    while (seci.hasMoreElements()) {
        secName = seci.peekNextKey();
        Ogre::ConfigFile::SettingsMultiMap *settings = seci.getNext();
        Ogre::ConfigFile::SettingsMultiMap::iterator i;
        for (i = settings->begin(); i != settings->end(); ++i) {
            typeName = i->first;
            archName = i->second;
            Ogre::ResourceGroupManager::getSingleton().addResourceLocation(archName, typeName,
                                                                           secName);
        }
    }
    Ogre::TextureManager::getSingleton().setDefaultNumMipmaps(5);
    Ogre::ResourceGroupManager::getSingleton().initialiseAllResourceGroups();

    // Set up timer.
    m_pTimer = new Ogre::Timer();
    m_pTimer->reset();

    // get the resource manager
    Ogre::ResourceGroupManager &resGroupMgr = Ogre::ResourceGroupManager::getSingleton();
    // tell it to look at this location
    resGroupMgr.addResourceLocation(FONT_FOLDER, "FileSystem");
    // get the font manager
    Ogre::FontManager &fontMgr = Ogre::FontManager::getSingleton();
    // create a font resource
    Ogre::ResourcePtr font = fontMgr.create("MyFont", "General");
    // set as truetype
    font->setParameter("type", "truetype");
    // set the .ttf file name
    font->setParameter("source", FONT_FILE_NAME);
    // set the size
    font->setParameter("size", "26");
    // set the dpi
    font->setParameter("resolution", "96");
    // load the ttf
    font->load();

    // Make a text overlay.
    Ogre::OverlayManager &overlayMgr = Ogre::OverlayManager::getSingleton();

    // Create a panel
    Ogre::OverlayContainer* panel =
        static_cast<Ogre::OverlayContainer*>(overlayMgr.createOverlayElement("Panel", "PanelName"));
    panel->setMetricsMode(Ogre::GMM_PIXELS);
    panel->setPosition(10, 10);
    panel->setDimensions(300, 120);

    // Create a text area
    m_debugText =
        static_cast<Ogre::TextAreaOverlayElement*>(overlayMgr.createOverlayElement("TextArea",
                                                                                   "TextAreaName"));
    m_debugText->setMetricsMode(Ogre::GMM_PIXELS);
    m_debugText->setPosition(0, 0);
    m_debugText->setDimensions(300, 120);
    m_debugText->setCharHeight(16);
    printf("setting font\n");
    m_debugText->setFontName("MyFont");
    std::cout << "font name '" << m_debugText->getFontName() << "'\n";
    // say something
    printf("setting caption\n");
    m_debugText->setCaption("");
    // set the font name to the font resource that you just created.
    // Create an overlay, and add the panel
    m_debugOverlay = overlayMgr.create("OverlayName");
    m_debugOverlay->add2D(panel);
    m_debugOverlay->show();

    // Add the text area to the panel
    printf("adding text to panel\n");
    panel->addChild(m_debugText);

    printf("finished making text overlay\n");

    // Set window active
    m_pRenderWnd->setActive(true);

    // Make a sphere which we'll use for focusing.
    m_focusSphere =
        OgreFramework::getSingletonPtr()->m_pSceneMgr->createEntity("focus_sphere",
                                                                    "sphere.mesh");
    m_focusSphereNode =
        OgreFramework::getSingletonPtr()->m_pSceneMgr->getRootSceneNode()->
        createChildSceneNode("focus_sphere_node");
    m_focusSphereNode->attachObject(m_focusSphere);
    m_focusSphereNode->setVisible(false);

    // set NED
    Ogre::Quaternion ogre2ned(sqrt(2)/2, sqrt(2)/2, 0, 0);
    OgreFramework::getSingletonPtr()->m_pSceneMgr->getRootSceneNode()->setOrientation(ogre2ned);

    // set initial camera
    setCamera();
    return true;
}


OgreFramework::~OgreFramework() {
    if (m_pInputMgr)
        OIS::InputManager::destroyInputSystem(m_pInputMgr);
    //    if (m_pTrayMgr)  delete m_pTrayMgr;
    if (m_pRoot)
        delete m_pRoot;
}


bool OgreFramework::keyPressed(const OIS::KeyEvent &keyEventRef __attribute__((unused))) {
    // std::cout << "key pressed: (" << keyEventRef.key << ", "
    //           << keyEventRef.text << ")" << std::endl;
    if (m_pKeyboard->isKeyDown(OIS::KC_ESCAPE)) {
        m_bShutDownOgre = true;
        return true;
    }

    if (m_pKeyboard->isKeyDown(OIS::KC_SYSRQ)) {
        m_pRenderWnd->writeContentsToTimestampedFile("BOF_Screenshot_", ".png");
        return true;
    }

    if (m_pKeyboard->isKeyDown(OIS::KC_M)) {
        static int mode = 0;

        if (mode == 2) {
            m_pCamera->setPolygonMode(Ogre::PM_SOLID);
            mode = 0;
        } else if (mode == 0) {
            m_pCamera->setPolygonMode(Ogre::PM_WIREFRAME);
            mode = 1;
        } else if (mode == 1) {
            m_pCamera->setPolygonMode(Ogre::PM_POINTS);
            mode = 2;
        }
    }

    if (m_pKeyboard->isKeyDown(OIS::KC_D)) {
        if (m_debugOverlay) {
            if (m_debugOverlay->isVisible()) {
                printf("hiding overlay\n");
                m_debugOverlay->hide();
            } else {
                printf("showing overlay\n");
                m_debugOverlay->show();
            }
        }
    }
    //  if (m_pKeyboard->isKeyDown(OIS::KC_O))
    //  {
    //    if (m_pTrayMgr->isLogoVisible())
    //    {
    //      m_pTrayMgr->hideLogo();
    //      m_pTrayMgr->hideFrameStats();
    //    }
    //    else
    //    {
    //      m_pTrayMgr->showLogo(OgreBites::TL_BOTTOMRIGHT);
    //      m_pTrayMgr->showFrameStats(OgreBites::TL_BOTTOMLEFT);
    //    }
    //  }

    return true;
}


bool OgreFramework::keyReleased(const OIS::KeyEvent &keyEventRef __attribute__((unused))) {
    return true;
}


bool OgreFramework::mouseMoved(const OIS::MouseEvent &evt) {
    //printf("mouse moved: dx: %d, dy: %d\n", evt.state.X.rel, evt.state.Y.rel);
    if (m_pMouse->getMouseState().buttonDown(OIS::MB_Left)) {
        m_cameraAzimuth   += 0.01 * evt.state.X.rel;
        m_cameraElevation -= 0.01 * evt.state.Y.rel;

        m_cameraElevation = std::min(0.5*M_PI, m_cameraElevation);
        m_cameraElevation = std::max(-0.5*M_PI, m_cameraElevation);
    }

    // scale radius by radius for dynamic range
    m_cameraRadius -= 0.0007*m_cameraRadius*evt.state.Z.rel;
    m_cameraRadius = std::max(1e-6, m_cameraRadius);

    if (m_pMouse->getMouseState().buttonDown(OIS::MB_Right)) {
        // scale translation for dynamic range
        double scale = 0.001 * m_cameraRadius;
        double dx = evt.state.X.rel;
        double dy = evt.state.Y.rel;
        double ca = cos(m_cameraAzimuth);
        double sa = sin(m_cameraAzimuth);
        m_cameraFocus.x -= scale*(dy*ca + dx*sa);
        m_cameraFocus.y -= scale*(dy*sa - dx*ca);
    }

    if (m_pMouse->getMouseState().buttonDown(OIS::MB_Middle)) {
        // scale translation for dynamic range
        double scale = 0.001 * m_cameraRadius;
        m_cameraFocus.z -= scale * evt.state.Y.rel;
    }

    double s = 1e-4*m_cameraRadius;
    m_focusSphereNode->setScale(s, s, s);
    m_focusSphereNode->setPosition(m_cameraFocus.x, m_cameraFocus.y, m_cameraFocus.z);

    return true;
}



bool OgreFramework::mousePressed(const OIS::MouseEvent &evt __attribute__((unused)),
                                 OIS::MouseButtonID id __attribute__((unused))) {
    // TODO(greg): This should be optional.
    m_focusSphereNode->setVisible(true);
    return true;
}

bool OgreFramework::mouseReleased(const OIS::MouseEvent &evt  __attribute__((unused)),
                                  OIS::MouseButtonID id __attribute__((unused))) {
    m_focusSphereNode->setVisible(false);
    return true;
}


void OgreFramework::setExtraMessages(char *msgs) {
    extra_messages = std::string(msgs);
}


void OgreFramework::updateOgre(double timeSinceLastFrame) {
    m_MoveScale = m_MoveSpeed   * static_cast<float>(timeSinceLastFrame);
    m_RotScale  = m_RotateSpeed * static_cast<float>(timeSinceLastFrame);

    m_TranslateVector = Ogre::Vector3::ZERO;

    getInput();
    setCamera();

    m_FrameEvent.timeSinceLastFrame = timeSinceLastFrame;
    //  m_pTrayMgr->frameRenderingQueued(m_FrameEvent);

    const Ogre::RenderTarget::FrameStats& stats = m_pRenderWnd->getStatistics();
    std::ostringstream os;

    os << "Average FPS: " << Ogre::StringConverter::toString(stats.avgFPS) << std::endl;
    os << "Current FPS: " << Ogre::StringConverter::toString(stats.lastFPS) << std::endl;
    os << "Best FPS: " << Ogre::StringConverter::toString(stats.bestFPS)
       << " (" << Ogre::StringConverter::toString(stats.bestFrameTime) << " ms)" << std::endl;
    os << "Worst FPS: " << Ogre::StringConverter::toString(stats.worstFPS)
       << " (" << Ogre::StringConverter::toString(stats.worstFrameTime) << " ms)" << std::endl;
    os << "Triangle Count: " << Ogre::StringConverter::toString(stats.triangleCount) << std::endl;
    os << "Batch Count: " << Ogre::StringConverter::toString(stats.batchCount) << std::endl;
    if (extra_messages.size() > 0) {
      os << extra_messages << std::endl;
    }

    m_debugText->setCaption(os.str());
}



void OgreFramework::setCamera() {
    double se = sin(m_cameraElevation);
    double ce = cos(m_cameraElevation);
    double sa = sin(m_cameraAzimuth);
    double ca = cos(m_cameraAzimuth);
    Ogre::Vector3 camera_pos_ned(m_cameraRadius*ce*ca, m_cameraRadius*ce*sa, m_cameraRadius*se);
    Ogre::Vector3 camera_pos_ogre(camera_pos_ned.x, -camera_pos_ned.z, camera_pos_ned.y);
    Ogre::Vector3 camera_focus_ogre(m_cameraFocus.x, -m_cameraFocus.z, m_cameraFocus.y);

    m_pCamera->setPosition(camera_focus_ogre + camera_pos_ogre);
    m_pCamera->setDirection(-camera_pos_ogre);

    //    m_pCamera->setNearClipDistance(1);
    //    m_pCamera->setFarClipDistance(1);
}


void OgreFramework::getInput() {
    if (m_pKeyboard->isKeyDown(OIS::KC_A))
        m_TranslateVector.x = -m_MoveScale;

    if (m_pKeyboard->isKeyDown(OIS::KC_D))
        m_TranslateVector.x = m_MoveScale;

    if (m_pKeyboard->isKeyDown(OIS::KC_W))
        m_TranslateVector.z = -m_MoveScale;

    if (m_pKeyboard->isKeyDown(OIS::KC_S))
        m_TranslateVector.z = m_MoveScale;
}
