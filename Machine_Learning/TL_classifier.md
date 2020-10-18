# TL classifier

Input: image, bbox input with neighbor indicator (N, 6), face presence, bulb mask (64, 64)

Note: num bulbs, bulb color, bulb type and positions are encoded in the bulb mask

Output: face bbox, face conf, bulb mask with bulb conf (64 , 64)

Do binary crossentropy over input and output bulb mask

Tend to predict as off bulb

face_presence:

1. `angled_bulb_not_visible`
2. `occluded`
3. `unclear`

## Multi-level class imbalance

1. ON-OFF, would rather predict as OFF rather than ON. Green false positive is critical event for
   us.
2. Bulb color
3. Face type, simple and complex
4. Foreground-background occlusion
5. neighboring cases
