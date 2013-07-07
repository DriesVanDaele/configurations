current_brightness=$(cat /sys/class/backlight/psb-bl/brightness)

if [ $current_brightness -lt 100 ]
then
    new_brightness=$(expr $current_brightness + 1)
    echo $new_brightness | sudo tee /sys/class/backlight/psb-bl/brightness
fi




