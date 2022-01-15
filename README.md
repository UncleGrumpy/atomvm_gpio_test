# gpio_test

Welcome to the gpio_test AtomVM application.

To build and flash this application to your ESP32 device with the AtomVM base image
alredy installed, issue the `esp32_flash` target

    shell$ rebar3 esp32_flash

  This does nothing useful, it was just written as an excercize/demonstration of using
more than one kind of GPIO device at the same time and passing messages among processes.
The led and button modules are designed to be reusable, but they need some more work to
be ready for general purpose use.
