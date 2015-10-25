//
//  ViewController.swift
//  TutorRemote
//
//  Created by Fabio Madge Pimentel on 23.10.15.
//  Copyright © 2015 Madge.ME. All rights reserved.
//

import Cocoa
import Starscream
import CoreGraphics
import AppKit

enum ANSI: CGKeyCode {
    case A                    = 0x00
    case S                    = 0x01
    case D                    = 0x02
    case F                    = 0x03
    case H                    = 0x04
    case G                    = 0x05
    case Z                    = 0x06
    case X                    = 0x07
    case C                    = 0x08
    case V                    = 0x09
    case B                    = 0x0B
    case Q                    = 0x0C
    case W                    = 0x0D
    case E                    = 0x0E
    case R                    = 0x0F
    case Y                    = 0x10
    case T                    = 0x11
    case _1                   = 0x12
    case _2                   = 0x13
    case _3                   = 0x14
    case _4                   = 0x15
    case _6                   = 0x16
    case _5                   = 0x17
    case Equal                = 0x18
    case _9                   = 0x19
    case _7                   = 0x1A
    case Minus                = 0x1B
    case _8                   = 0x1C
    case _0                   = 0x1D
    case RightBracket         = 0x1E
    case O                    = 0x1F
    case U                    = 0x20
    case LeftBracket          = 0x21
    case I                    = 0x22
    case P                    = 0x23
    case L                    = 0x25
    case J                    = 0x26
    case Quote                = 0x27
    case K                    = 0x28
    case Semicolon            = 0x29
    case Backslash            = 0x2A
    case Comma                = 0x2B
    case Slash                = 0x2C
    case N                    = 0x2D
    case M                    = 0x2E
    case Period               = 0x2F
    case Grave                = 0x32
    case KeypadDecimal        = 0x41
    case KeypadMultiply       = 0x43
    case KeypadPlus           = 0x45
    case KeypadClear          = 0x47
    case KeypadDivide         = 0x4B
    case KeypadEnter          = 0x4C
    case KeypadMinus          = 0x4E
    case KeypadEquals         = 0x51
    case Keypad0              = 0x52
    case Keypad1              = 0x53
    case Keypad2              = 0x54
    case Keypad3              = 0x55
    case Keypad4              = 0x56
    case Keypad5              = 0x57
    case Keypad6              = 0x58
    case Keypad7              = 0x59
    case Keypad8              = 0x5B
    case Keypad9              = 0x5C
};

/* keycodes for keys that are independent of keyboard layout*/
enum Independent: CGKeyCode {
    case Return                    = 0x24
    case Tab                       = 0x30
    case Space                     = 0x31
    case Delete                    = 0x33
    case Escape                    = 0x35
    case Command                   = 0x37
    case Shift                     = 0x38
    case CapsLock                  = 0x39
    case Option                    = 0x3A
    case Control                   = 0x3B
    case RightShift                = 0x3C
    case RightOption               = 0x3D
    case RightControl              = 0x3E
    case Function                  = 0x3F
    case F17                       = 0x40
    case VolumeUp                  = 0x48
    case VolumeDown                = 0x49
    case Mute                      = 0x4A
    case F18                       = 0x4F
    case F19                       = 0x50
    case F20                       = 0x5A
    case F5                        = 0x60
    case F6                        = 0x61
    case F7                        = 0x62
    case F3                        = 0x63
    case F8                        = 0x64
    case F9                        = 0x65
    case F11                       = 0x67
    case F13                       = 0x69
    case F16                       = 0x6A
    case F14                       = 0x6B
    case F10                       = 0x6D
    case F12                       = 0x6F
    case F15                       = 0x71
    case Help                      = 0x72
    case Home                      = 0x73
    case PageUp                    = 0x74
    case ForwardDelete             = 0x75
    case F4                        = 0x76
    case End                       = 0x77
    case F2                        = 0x78
    case PageDown                  = 0x79
    case F1                        = 0x7A
    case LeftArrow                 = 0x7B
    case RightArrow                = 0x7C
    case DownArrow                 = 0x7D
    case UpArrow                   = 0x7E
};


class ViewController: NSViewController, WebSocketDelegate {
    var socket = WebSocket(url: NSURL(string: "ws://tutorremote.madge.me:9160")!, protocols: ["tutorremote"])
    var shiftState = false
    var ctrlState = false
    var altState = false
    var cmdState = false
    var wireToUSANSI : [String: ANSI] = ["U+0041": ANSI.A, "U+0053": ANSI.S, "U+0044": ANSI.D, "U+0046": ANSI.F, "U+0048": ANSI.H, "U+0047": ANSI.G, "U+005A": ANSI.Z, "U+0058": ANSI.X, "U+0043": ANSI.C, "U+0056": ANSI.V, "U+0042": ANSI.B, "U+0051": ANSI.Q, "U+0057": ANSI.W, "U+0045": ANSI.E, "U+0052": ANSI.R, "U+0059": ANSI.Y, "U+0054": ANSI.T, "U+0031": ANSI._1, "U+0032": ANSI._2, "U+0033": ANSI._3, "U+0034": ANSI._4, "U+0036": ANSI._6, "U+0035": ANSI._5, "U+003D": ANSI.Equal, "U+0039": ANSI._9, "U+0037": ANSI._7, "U+002D": ANSI.Minus, "U+0038": ANSI._8, "U+0030": ANSI._0, "U+005D": ANSI.RightBracket, "U+004F": ANSI.O, "U+0055": ANSI.U, "U+005B": ANSI.LeftBracket, "U+0049": ANSI.I, "U+0050": ANSI.P, "U+004C": ANSI.L, "U+004A": ANSI.J, "U+0027": ANSI.Quote, "U+004B": ANSI.K, "U+003B": ANSI.Semicolon, "U+005C": ANSI.Backslash, "U+002C": ANSI.Comma, "U+002F": ANSI.Slash, "U+004E": ANSI.N, "U+004D": ANSI.M, "U+002E": ANSI.Period, "U+0060": ANSI.Grave]
    var wireToIndependent : [String: Independent] = ["Enter":  Independent.Return, "U+0009": Independent.Tab, "U+0020": Independent.Space, "U+0008": Independent.Delete, "U+001B": Independent.Escape, "Meta": Independent.Command, "Shift": Independent.Shift, "CapsLock": Independent.CapsLock, "Alt": Independent.Option, "Control": Independent.Control, "Home": Independent.Home, "PageUp": Independent.PageUp, "U+007F": Independent.ForwardDelete, "End": Independent.End, "PageDown": Independent.PageDown, "Left": Independent.LeftArrow, "Right": Independent.RightArrow, "Down": Independent.DownArrow, "Up": Independent.UpArrow]

    override func viewDidLoad() {
        super.viewDidLoad()

        // Do any additional setup after loading the view.
    }

    override var representedObject: AnyObject? {
        didSet {
//         Update the view, if already loaded.
        }
    }

    @IBOutlet weak var token: NSTextField!
    @IBAction func updateFilter(sender: AnyObject) {
        if self.socket.isConnected { self.socket.writeString("FLUP: " + token.stringValue) }
    }

    @IBAction func weakConnect(sender: AnyObject) {
        if !self.socket.isConnected {
            connect(sender)
        }
    }

    @IBAction func connect(sender: AnyObject) {
        if self.socket.isConnected {
            print("disconnect")
            statusField.stringValue = "Disconnecting..."
            socket.disconnect()
        }
        else if let checkedUrl = NSURL(string: self.urlField.stringValue) {
            print("connect")
            socket.disconnect()
            socket = WebSocket(url: checkedUrl, protocols: ["tutorremote"])
            socket.delegate = self
            statusField.stringValue = "Connecting..."
            socket.connect()
        }
        
    }

    @IBOutlet weak var urlField: NSTextField!

    @IBOutlet weak var statusField: NSTextField!
    
    func websocketDidConnect(ws: WebSocket) {
        print("websocket is connected")
        self.connectButton.title = "Disconnect"
        statusField.stringValue = "Channel open"
        self.socket.writeString("OUTPUTINIT")
    }
    @IBOutlet weak var connectButton: NSButton!
    
    func websocketDidDisconnect(ws: WebSocket, error: NSError?) {
        if let e = error {
            print("websocket is disconnected: \(e.localizedDescription)")
        } else {
            print("websocket disconnected")
        }
        self.connectButton.title = "Connect";
        statusField.stringValue = "Not Connected"
    }
    
    func websocketDidReceiveMessage(ws: WebSocket, text: String) {
        let payload = text.substringFromIndex(text.startIndex.advancedBy(6))
        //keyDown
        if text.hasPrefix("char: "){
            if let ansiKey = wireToUSANSI[payload]{
                print(payload + " " + shiftState.description + " " + ctrlState.description + " " + altState.description + " " + cmdState.description)
                sendKeyToSystem(ansiKey.rawValue, shift: shiftState, ctrl: ctrlState, alt: altState, cmd: cmdState)
            }
            else if let indieKey = wireToIndependent[payload]{
                switch payload{
                case "Shift": shiftState = true
                case "Control": ctrlState = true
                case "Alt": altState = true
                case "Meta": cmdState = true
                default: sendKeyToSystem(indieKey.rawValue, shift: shiftState, ctrl: ctrlState, alt: altState, cmd: cmdState)
                }
            }
        }
        //keyup
        else if text.hasPrefix("CHAR: "){
                switch payload{
                case "Shift": shiftState = false
                case "Control": ctrlState = false
                case "Alt": altState = false
                case "Meta": cmdState = false
                default: break
            }
        }
        else if text.hasPrefix("TEXT: "){
            let board = NSPasteboard.generalPasteboard()
            board.declareTypes([NSStringPboardType], owner: nil)
            board.setString(payload, forType: NSStringPboardType)
            
            //cmd+V
            sendKeyToSystem(ANSI.V.rawValue, shift: false, ctrl: false, alt: false, cmd: true)
        }
        else if text.hasPrefix("FLUP: "){
            if token.stringValue != payload {
                token.stringValue = payload
            }
        }
        else if text.hasPrefix("TOKN: "){
            statusField.stringValue = "Connected as " + payload
        }
        else{
            print("Did not recognise the following message: " + text)
        }
    }

    
    func websocketDidReceiveData(ws: WebSocket, data: NSData) {
        print("Received data: \(data.length)")
    }
    
    @IBOutlet weak var input: NSButton!
    func sendKeyToSystem(key: CGKeyCode, shift: Bool, ctrl: Bool, alt: Bool, cmd: Bool){
        if (input.state != 1) { return }
        let src = CGEventSourceCreate(CGEventSourceStateID.CombinedSessionState)

        let event1 = CGEventCreateKeyboardEvent(src, key, true)
        if(shift) {CGEventSetFlags(event1, CGEventFlags.MaskShift)}
        if(ctrl) {CGEventSetFlags(event1, CGEventFlags.MaskControl)}
        if(alt) {CGEventSetFlags(event1, CGEventFlags.MaskAlternate)}
        if(cmd) {CGEventSetFlags(event1, CGEventFlags.MaskCommand)}
        
        let event2 = CGEventCreateKeyboardEvent(src, key, false)
        if(shift) {CGEventSetFlags(event2, CGEventFlags.MaskShift)}
        if(ctrl) {CGEventSetFlags(event2, CGEventFlags.MaskControl)}
        if(alt) {CGEventSetFlags(event2, CGEventFlags.MaskAlternate)}
        if(cmd) {CGEventSetFlags(event2, CGEventFlags.MaskCommand)}
        
        CGEventPost(CGEventTapLocation.CGAnnotatedSessionEventTap, event1)
        CGEventPost(CGEventTapLocation.CGAnnotatedSessionEventTap, event2)
    }
}

