package realClassOne.chickenStock.auth.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;

@Controller
@RequiredArgsConstructor
public class TestController {
    @GetMapping("/oauth2/callback")
    public String oauthCallback(@RequestParam(required = false) String token) {
        return "callback";
    }
}
