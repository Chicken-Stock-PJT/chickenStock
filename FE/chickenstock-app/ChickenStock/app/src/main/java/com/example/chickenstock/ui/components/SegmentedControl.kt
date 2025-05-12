package com.example.chickenstock.ui.components

import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material3.Surface
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Brush
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import com.example.chickenstock.ui.theme.SCDreamFontFamily
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.runtime.remember

@Composable
fun SegmentedControl(
    items: List<String>,
    selectedIndex: Int,
    onSelectedIndexChange: (Int) -> Unit,
    modifier: Modifier = Modifier
) {
    val yellow = Color(0xFFFDD141)
    Surface(
        modifier = modifier.fillMaxWidth(),
        shape = RoundedCornerShape(20.dp),
        color = yellow
    ) {
        Row(
            modifier = Modifier.padding(4.dp)
        ) {
            items.forEachIndexed { index, item ->
                Box(
                    modifier = Modifier
                        .weight(1f)
                        .height(36.dp)
                        .background(
                            color = if (selectedIndex == index) Color.White.copy(alpha = 0.2f) else Color.Transparent,
                            shape = RoundedCornerShape(16.dp)
                        )
                        .clickable(
                            indication = null,
                            interactionSource = remember { MutableInteractionSource() }
                        ) { onSelectedIndexChange(index) },
                    contentAlignment = Alignment.Center
                ) {
                    Box(
                        modifier = Modifier
                            .fillMaxSize()
                            .background(
                                color = if (selectedIndex == index) Color.White else Color.Transparent,
                                shape = RoundedCornerShape(16.dp)
                            )
                    ) {
                        Text(
                            text = item,
                            color = Color.Black,
                            fontFamily = SCDreamFontFamily,
                            fontWeight = if (selectedIndex == index) FontWeight.Bold else FontWeight.Normal,
                            modifier = Modifier.align(Alignment.Center)
                        )
                    }
                }
            }
        }
    }
} 