# Anti-Fingerprinting Implementation for Adidas Scraper

This document outlines our comprehensive anti-fingerprinting implementation based on research from [Apify's anti-scraping techniques](https://docs.apify.com/academy/anti-scraping/techniques/fingerprinting).

## Overview

Browser fingerprinting is a technique used by websites to collect unique information about a browser's configuration, which can be used to track users or block automated scrapers. Our implementation mocks all major fingerprinting vectors to avoid detection.

## Fingerprinting Vectors Covered

### 1. HTTP Headers
- **User-Agent**: Mocked with realistic browser strings
- **Accept**: Content types the browser can render
- **Content-Language**: User's preferred language
- **Referer**: Previous page address

### 2. Window Properties (navigator object)
- **userAgent**: Browser and OS information
- **platform**: Operating system platform
- **cookieEnabled**: Cookie support status
- **doNotTrack**: Privacy settings
- **productSub**: Layout engine version
- **hardwareConcurrency**: Number of CPU cores
- **deviceMemory**: Available RAM
- **languages**: User's language preferences
- **vendor**: Browser vendor information
- **connection**: Network connection details
- **maxTouchPoints**: Touch support
- **userActivation**: User interaction state

### 3. Screen Properties
- **screen.width/height**: Screen resolution
- **screen.availWidth/availHeight**: Available screen space
- **screen.colorDepth**: Color depth
- **screen.pixelDepth**: Pixel depth

### 4. Audio/Video Codec Support
- **HTMLMediaElement.canPlayType()**: Audio codec support
- **HTMLVideoElement.canPlayType()**: Video codec support
- Mocked responses: "probably", "maybe", or "" for unsupported

### 5. WebGL Fingerprinting
- **WebGLRenderingContext.getParameter()**: Graphics card information
- Mocked vendor and renderer strings
- Parameters 37445 (vendor) and 37446 (renderer)

### 6. Canvas Fingerprinting
- **CanvasRenderingContext2D.getImageData()**: Canvas pixel data
- Adds subtle random noise to prevent consistent fingerprinting

### 7. AudioContext Fingerprinting
- **AudioBuffer.getChannelData()**: Audio processing data
- Adds minimal random noise to audio samples

### 8. Additional APIs
- **BatteryManager**: Battery status information
- **Permissions API**: Permission query responses
- **MediaDevices**: Camera/microphone enumeration
- **Gamepad API**: Gamepad detection
- **VR API**: Virtual reality display detection
- **Chrome Runtime**: Chrome-specific APIs

### 9. Enhanced Plugin and MIME Type Fingerprinting
- **Detailed Plugin Names**: Complex plugin identifiers with encoded strings
- **MIME Type Support**: Detailed MIME type configurations
- **Multimedia Device Counts**: Speaker, microphone, and webcam counts
- **Canvas Touch Points**: Touch point detection patterns (tpCanvas)

### 10. Advanced Fingerprinting Properties
- **multimediaDevices**: Object with speakers, micros, and webcams counts
- **tpCanvas**: Touch point canvas fingerprint with specific patterns
- **Enhanced Plugins**: Realistic plugin names with encoded identifiers
- **Detailed MIME Types**: Complex MIME type strings for PDF support

## Implementation Files

### 1. `src/Adidas/AntiFingerprint.hs`
Contains the core anti-fingerprinting JavaScript patches:
- `mockComprehensiveFingerprint`: Main fingerprint mocking function
- `setupAntiFingerprinting`: Complete anti-fingerprinting setup
- `randomizeUserAgent`: User-Agent randomization
- `addRandomDelays`: Human-like timing
- `humanizeScrolling`: Natural scrolling behavior

### 2. `src/Adidas/FingerprintConfig.hs`
Provides configurable fingerprint profiles:
- `FingerprintProfile`: Data structure for fingerprint configuration
- `defaultProfile`: Firefox on Linux profile
- `firefoxProfile`: Firefox-specific profile
- `chromeProfile`: Chrome-specific profile
- `safariProfile`: Safari-specific profile

### 3. `Main.hs`
Main application with CLI support:
- Profile selection via `--profile` option
- Integration with anti-fingerprinting measures
- Enhanced Chrome configuration

## Usage Examples

### Basic Usage
```bash
# Use default fingerprint profile
bazel run //haskell/app/adidas-scraper:adidas-scraper-with-selenium -- \
  scrape-page --url "https://www.adidas.com/us/men-shoes" \
  --output products.json --delay 3
```

### Profile Selection
```bash
# Use Firefox profile
bazel run //haskell/app/adidas-scraper:adidas-scraper-with-selenium -- \
  scrape-page --url "https://www.adidas.com/us/men-shoes" \
  --output products.json --delay 3 --profile firefox

# Use Chrome profile
bazel run //haskell/app/adidas-scraper:adidas-scraper-with-selenium -- \
  scrape-page --url "https://www.adidas.com/us/men-shoes" \
  --output products.json --delay 3 --profile chrome

# Use Safari profile
bazel run //haskell/app/adidas-scraper:adidas-scraper-with-selenium -- \
  scrape-page --url "https://www.adidas.com/us/men-shoes" \
  --output products.json --delay 3 --profile safari
```

## Fingerprint Profile Examples

### Default Profile (Firefox on Linux)
```json
{
  "userAgent": "Mozilla/5.0 (X11; Linux x86_64; rv:90.0) Gecko/20100101 Firefox/90.0",
  "platform": "Linux x86_64",
  "vendor": "Google Inc.",
  "product": "Gecko",
  "productSub": "20100101",
  "hardwareConcurrency": 8,
  "deviceMemory": 8,
  "languages": ["en-US", "en"],
  "screenWidth": 1920,
  "screenHeight": 1080,
  "colorDepth": 24,
  "timezone": "Europe/Prague",
  "webglVendor": "Intel Open Source Technology Center",
  "webglRenderer": "Mesa DRI Intel(R) HD Graphics 4600 (HSW GT2)",
  "audioCodecs": {
    "audio/ogg": "probably",
    "audio/mp3": "maybe",
    "audio/wav": "probably",
    "audio/m4a": "maybe",
    "audio/aac": "maybe"
  },
  "videoCodecs": {
    "video/ogg": "probably",
    "video/h264": "probably",
    "video/webm": "probably"
  },
  "plugins": [
    "VKkaVKs1::T05FCozZz4k5cOmyCBn6laNl5kx3bVKk::DozZz4cOuf2bNteP::__~q0i~DJMtWyhQQvAny4k5kx3j47d1asWq8Hi4",
    "JavaScript Portable Document Format Viewer::Portable Document Format::QQnTRQv2Epc1iZUxgvXTwBIr0a0DgYz4::__application/x-google-chrome-pdf~pdf~Portable Document Format",
    "NtWq0a0::z4cOuXToUSRQvAny4kx3j4FCgQIMlx3::8DBn6laVKs9e2jw::__~FOP~FCgQIMlxBIr0a0DgYrVxYMGi4FKFhvfu",
    "Web com.adobe.pdf Renderer::::d1a0DgYrdOufuAny4kxBIr0asWq0asWy::__application/pdf~pdf~"
  ],
  "mimeTypes": [
    "~~application/pdf~~pdf",
    "Portable Document Format~~application/x-google-chrome-pdf~~pdf"
  ],
  "multimediaDevices": {
    "speakers": 1,
    "micros": 1,
    "webcams": 1
  },
  "tpCanvas": {
    "0": 0,
    "1": 1,
    "2": 1,
    "3": 0
  },
  "connectionType": "4g",
  "connectionDownlink": 10,
  "maxTouchPoints": 0
}
```

## Testing

Use the test script to verify fingerprint mocking:
```bash
# Build and run the test
ghc test_fingerprint.hs -o test_fingerprint
./test_fingerprint
```

The test will open a browser to https://bot.sannysoft.com, which provides a comprehensive fingerprint detection test.

## Advanced Techniques

### 1. Session Rotation
- Rotate fingerprint profiles between requests
- Use different User-Agent strings
- Vary screen resolutions and timezones

### 2. Proxy Integration
- Combine with residential proxies
- Match fingerprint profiles to proxy locations
- Rotate sessions with proxies

### 3. Behavioral Patterns
- Random delays between actions
- Human-like scrolling patterns
- Mouse movement simulation
- Natural page interaction timing

## Limitations and Considerations

1. **Dynamic Detection**: Some sites may use additional detection methods
2. **JavaScript Execution**: All patches rely on JavaScript execution
3. **Timing Attacks**: Advanced sites may detect automation through timing
4. **Hardware Fingerprinting**: Some techniques may still reveal hardware differences

## Future Enhancements

1. **Undetected Chrome**: Integration with undetected-chromedriver
2. **Proxy Rotation**: Automatic proxy management
3. **Behavioral AI**: Machine learning for human-like behavior
4. **Fingerprint Evolution**: Dynamic fingerprint generation
5. **Stealth Mode**: Additional stealth techniques

## References

- [Apify Anti-Scraping Academy - Fingerprinting](https://docs.apify.com/academy/anti-scraping/techniques/fingerprinting)
- [Browser Fingerprinting Detection](https://bot.sannysoft.com)
- [WebDriver Stealth Techniques](https://github.com/berstend/puppeteer-extra/tree/master/packages/puppeteer-extra-plugin-stealth) 