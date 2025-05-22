package com.example.chickenstock.navigation

import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.navigation.NavHostController
import androidx.navigation.NavType
import androidx.navigation.compose.NavHost
import androidx.navigation.compose.composable
import androidx.navigation.navArgument
import com.example.chickenstock.ui.screens.stock.StockScreen
import com.example.chickenstock.ui.screens.Home.HomeScreen
import com.example.chickenstock.ui.screens.mypage.MyPageScreen
import com.example.chickenstock.viewmodel.MainViewModel
import com.example.chickenstock.ui.screens.stock.StockDetailScreen
import com.example.chickenstock.ui.components.StockItem
import com.example.chickenstock.ui.screens.search.SearchScreen
import com.example.chickenstock.ui.screens.login.LoginScreen
import com.example.chickenstock.ui.screens.login.SignupScreen
import com.example.chickenstock.ui.screens.login.TermsAgreementScreen
import com.example.chickenstock.ui.screens.mypage.SettingScreen
import com.example.chickenstock.viewmodel.AuthViewModel
import androidx.compose.ui.platform.LocalContext
import androidx.lifecycle.viewmodel.compose.viewModel
import com.example.chickenstock.ui.screens.login.FindPWScreen
import com.example.chickenstock.ui.screens.login.VerificationScreen
import com.example.chickenstock.ui.screens.login.SignupSuccessScreen
import com.example.chickenstock.api.MemberService
import com.example.chickenstock.api.StockService
import com.example.chickenstock.api.RetrofitClient
import com.example.chickenstock.ui.screens.SplashScreen
import com.example.chickenstock.ui.screens.ranking.RankingScreen
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.material3.Text
import androidx.compose.foundation.layout.Box
import androidx.compose.ui.Alignment
import androidx.compose.foundation.layout.fillMaxSize
import com.example.chickenstock.ui.screens.stock.ChatScreen
import com.example.chickenstock.ui.screens.ranking.AIDashboardScreen

sealed class Screen(val route: String) {
    object Home : Screen("home")
    object Stock : Screen("stock")
    object MyPage : Screen("mypage")
    object Search : Screen("search")
    object Login : Screen("login")
    object Signup : Screen("signup")
    object Setting : Screen("setting")
    object FindPW : Screen("findpw")
    object Verification : Screen("verification")
    object SignupSuccess : Screen("signup_success")
    object TermsAgreement : Screen("terms_agreement")
    object StockDetail : Screen("stock_detail/{stockCode}/{currentPrice}/{fluctuationRate}") {
        fun createRoute(stockCode: String, currentPrice: String, fluctuationRate: String) = 
            "stock_detail/$stockCode/$currentPrice/$fluctuationRate"
    }
    object Splash : Screen("splash")
    object Ranking : Screen("ranking")
    object Chat : Screen("chat/{stockCode}")
}

@Composable
fun NavGraph(
    navController: NavHostController,
    modifier: Modifier = Modifier,
    viewModel: MainViewModel,
    authViewModel: AuthViewModel = viewModel(factory = AuthViewModel.Factory(LocalContext.current))
) {
    val context = LocalContext.current
    val memberService = RetrofitClient.getInstance(context).create(MemberService::class.java)
    val stockService = RetrofitClient.getInstance(context).create(StockService::class.java)

    NavHost(
        navController = navController,
        startDestination = Screen.Splash.route,
        modifier = modifier
    ) {
        composable(Screen.Splash.route) {
            SplashScreen(
                navController = navController,
                authViewModel = authViewModel,
                viewModel = viewModel
            )
        }
        
        composable(Screen.Home.route) {
            HomeScreen(
                navController = navController,
                authViewModel = authViewModel,
                memberService = memberService,
                stockService = stockService,
                viewModel = viewModel
            )
        }
        composable(Screen.Stock.route) {
            StockScreen(
                navController = navController,
                authViewModel = authViewModel,
                viewModel = viewModel
            )
        }
        composable(
            route = Screen.MyPage.route + "?tab={tab}&pending={pending}",
            arguments = listOf(
                navArgument("tab") {
                    type = NavType.StringType
                    nullable = true
                },
                navArgument("pending") {
                    type = NavType.StringType
                    nullable = true
                    defaultValue = null
                }
            )
        ) { entry ->
            MyPageScreen(
                navController = navController,
                navBackStackEntry = entry,
                authViewModel = authViewModel,
                mainViewModel = viewModel
            )
        }
        composable(Screen.Search.route) {
            SearchScreen(
                navController = navController
            )
        }
        composable(Screen.Login.route) {
            LoginScreen(
                navController = navController,
                authViewModel = authViewModel
            )
        }
        composable(Screen.TermsAgreement.route) {
            TermsAgreementScreen(
                navController = navController,
                viewModel = viewModel
            )
        }
        composable(Screen.Signup.route) {
            SignupScreen(
                navController = navController
            )
        }
        composable(Screen.Setting.route) {
            SettingScreen(
                navController = navController,
                viewModel = viewModel,
                authViewModel = authViewModel
            )
        }
        composable(Screen.FindPW.route) {
            FindPWScreen(
                navController = navController
            )
        }
        composable(
            route = Screen.StockDetail.route,
            arguments = listOf(
                navArgument("stockCode") { type = NavType.StringType },
                navArgument("currentPrice") { type = NavType.StringType },
                navArgument("fluctuationRate") { type = NavType.StringType }
            )
        ) { backStackEntry ->
            val stockCode = backStackEntry.arguments?.getString("stockCode") ?: ""
            val currentPrice = backStackEntry.arguments?.getString("currentPrice") ?: ""
            val fluctuationRate = backStackEntry.arguments?.getString("fluctuationRate") ?: ""
            
            val stock = StockItem(
                stockCode = stockCode,
                stockName = ".",
                market = "KOSPI",
                currentPrice = currentPrice,
                fluctuationRate = fluctuationRate,
                tradeAmount = "950"
            )
            StockDetailScreen(
                navController = navController, 
                stock = stock,
                viewModel = viewModel,
                authViewModel = authViewModel
            )
        }
        composable(
            route = "verification?email={email}&name={name}&nickname={nickname}&password={password}",
            arguments = listOf(
                navArgument("email") { type = NavType.StringType },
                navArgument("name") { type = NavType.StringType },
                navArgument("nickname") { type = NavType.StringType },
                navArgument("password") { type = NavType.StringType }
            )
        ) { entry ->
            VerificationScreen(
                navController = navController,
                email = entry.arguments?.getString("email") ?: "",
                name = entry.arguments?.getString("name") ?: "",
                nickname = entry.arguments?.getString("nickname") ?: "",
                password = entry.arguments?.getString("password") ?: "",
                viewModel = viewModel
            )
        }
        composable(Screen.SignupSuccess.route) {
            SignupSuccessScreen(
                navController = navController,
                viewModel = viewModel
            )
        }
        composable(Screen.Ranking.route) {
            RankingScreen(
                navController = navController
            )
        }
        composable(
            route = "chat/{stockCode}",
            arguments = listOf(
                navArgument("stockCode") { type = NavType.StringType }
            )
        ) { backStackEntry ->
            val stockCode = backStackEntry.arguments?.getString("stockCode") ?: ""
            ChatScreen(stockCode = stockCode, viewModel = viewModel, navController = navController, authViewModel = authViewModel)
        }
        composable(
            route = "ai_dashboard?nickname={nickname}&memberId={memberId}",
            arguments = listOf(
                navArgument("nickname") { type = NavType.StringType; nullable = true },
                navArgument("memberId") { type = NavType.StringType; nullable = true }
            )
        ) { entry ->
            AIDashboardScreen(navController = navController)
        }
    }
} 