sealed class Screen(val route: String) {
    object Splash : Screen("splash")
    object Home : Screen("home")
    object Login : Screen("login")
    object Signup : Screen("signup")
    object Detail : Screen("detail")
    object Stock : Screen("stock")
    object Search : Screen("search")
    object MyPage : Screen("mypage")
    object FindPW : Screen("findpw")
    object GoogleSignup : Screen("google_signup")
    object KakaoSignup : Screen("kakao_signup")
    object AppleSignup : Screen("apple_signup")
} 