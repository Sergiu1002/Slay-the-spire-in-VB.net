Public Class Form1
    Public hp, hp_total, strenght, block, enemy_block, energy, totalenergy, floor, cards_total, cards_discarded, hp_enemy, hp_enemy_total, enemy_action, contor_eweak, contor_yweak, contor_evulnerable, contor_yvulnerable, contor_bloodletting, dmg, shaker, cox, flex_contor As Integer
    Dim vulnerable, weak, barricade, battletrance, berserk, brutality, demon_form, combust, flame_barrier As Boolean
    Dim yname As String
    Dim last As Integer

    Dim mesaj, titlu, primordial As String

    'carduri_joc = cartile care s in mana
    Dim carduri_joc As List(Of Integer) = New List(Of Integer)()

    'carduri_accesibile = cardurile la care ai access-carduri_joc
    Dim carduri_accesibile As List(Of Integer) = New List(Of Integer)()

    'carduri_functie = ce functie de card trebuie sa execute pe click
    Dim carduri_functie As List(Of Integer) = New List(Of Integer)()
    'Dim carduri_exhaust As List(Of Integer) = New List(Of Integer)()

    'carduri_noi = ce carduri se adauga pe parcurs(dupa floor)
    Dim carduri_noi As List(Of Integer) = New List(Of Integer)()

    'cardurile anger
    Dim carduri_anger As List(Of Integer) = New List(Of Integer)()

    'ce carduri is pe ce pozitie pt a afla unde poate trage next card if card trebuie de tras
    Dim carduri_draw_space As List(Of Integer) = New List(Of Integer)()
    Function draw_pointer() As Integer
        carduri_draw_space.Clear()
        If PictureBox9.Image Is Nothing Then
            carduri_draw_space.Add(0)
        End If
        If PictureBox10.Image Is Nothing Then
            carduri_draw_space.Add(1)
        End If
        If PictureBox11.Image Is Nothing Then
            carduri_draw_space.Add(2)
        End If
        If PictureBox12.Image Is Nothing Then
            carduri_draw_space.Add(3)
        End If
        If PictureBox13.Image Is Nothing Then
            carduri_draw_space.Add(4)
        End If
        If PictureBox14.Image Is Nothing Then
            carduri_draw_space.Add(5)
        End If
        If PictureBox15.Image Is Nothing Then
            carduri_draw_space.Add(6)
        End If
        If PictureBox16.Image Is Nothing Then
            carduri_draw_space.Add(7)
        End If
        If PictureBox17.Image Is Nothing Then
            carduri_draw_space.Add(8)
        End If
        If PictureBox18.Image Is Nothing Then
            carduri_draw_space.Add(9)
        End If
        If PictureBox19.Image Is Nothing Then
            carduri_draw_space.Add(10)
        End If
    End Function
    Function drawer_vechi() As Integer
        If PictureBox9.Image Is Nothing Then
            draw1()
            Return 1
        End If
        If PictureBox10.Image Is Nothing Then
            draw2()
            Return 1
        End If
        If PictureBox11.Image Is Nothing Then
            draw3()
            Return 1
        End If
        If PictureBox12.Image Is Nothing Then
            draw4()
            Return 1
        End If
        If PictureBox13.Image Is Nothing Then
            draw5()
            Return 1
        End If
        If PictureBox14.Image Is Nothing Then
            draw6()
            Return 1
        End If
        If PictureBox15.Image Is Nothing Then
            draw7()
            Return 1
        End If
        If PictureBox16.Image Is Nothing Then
            draw8()
            Return 1
        End If
        If PictureBox17.Image Is Nothing Then
            draw9()
            Return 1
        End If
        If PictureBox18.Image Is Nothing Then
            draw10()
            Return 1
        End If
        If PictureBox19.Image Is Nothing Then
            draw11()
            Return 1
        End If
    End Function
    Function drawer() As Integer
        If carduri_draw_space.Count() Then
            Select Case carduri_draw_space(0)
                Case 0
                    draw1()
                    Return 1
                Case 1
                    draw2()
                    Return 1
                Case 2
                    draw3()
                    Return 1
                Case 3
                    draw4()
                    Return 1
                Case 4
                    draw5()
                    Return 1
                Case 5
                    draw6()
                    Return 1
                Case 6
                    draw7()
                    Return 1
                Case 7
                    draw8()
                    Return 1
                Case 8
                    draw9()
                    Return 1
                Case 9
                    draw10()
                    Return 1
                Case 10
                    draw11()
                    Return 1
            End Select
        End If

    End Function
    Function random() As Integer
        Randomize()
        Return Int((10 * Rnd()) + 1)
    End Function
    Function random_card() As Integer
        Randomize()
        cox = CInt(Math.Floor((26 - 3 + 1) * Rnd())) + 3
        If cox = 15 Or cox = 16 Then
            random_card()
        End If
    End Function
    Function shake() As Integer
        If shaker >= 250 Then
            shaker = 0
        End If

        Do Until shaker = 250

            Me.Left -= 7
            Me.Left += 7
            shaker += 1
        Loop
    End Function
    Function Shuffle(Of T)(ByVal list As IList(Of T))
        Dim r As Random = New Random()
        For i = 0 To list.Count - 1
            Dim index As Integer = r.Next(i, list.Count)
            If i <> index Then
                ' swap list(i) and list(index)
                Dim temp As T = list(i)
                list(i) = list(index)
                list(index) = temp
            End If
        Next
    End Function
    Function deckupgrade() As Integer
        If carduri_noi.Count() >= 1 Then

            For Each x As Integer In carduri_noi
                carduri_accesibile.Add(x)
            Next
        End If

    End Function
    Function startingdeck() As Integer
        carduri_accesibile.Clear()
        carduri_joc.Clear()
        carduri_accesibile.Add(0)
        carduri_accesibile.Add(0)
        carduri_accesibile.Add(0)
        carduri_accesibile.Add(0)
        carduri_accesibile.Add(0)
        carduri_accesibile.Add(1)
        carduri_accesibile.Add(1)
        carduri_accesibile.Add(1)
        carduri_accesibile.Add(1)
        carduri_accesibile.Add(1)
        carduri_accesibile.Add(2)
    End Function
    Function draw_card() As Integer
        carduri_joc.Add(carduri_accesibile(last))
        carduri_functie.Add(carduri_accesibile(last))
        carduri_accesibile.RemoveAt(last)
        last = carduri_accesibile.Count - 1
        If last <= -1 Then
            For Each x As Integer In carduri_anger
                carduri_accesibile.Add(x)
            Next
            startingdeck()
            deckupgrade()
            Shuffle(carduri_accesibile)
        End If
    End Function
    Function ydmg(ByVal a As Integer, ByVal b As Integer) As Integer
        Dim temp As Integer
        temp = -a + (b + strenght)
        If temp <= 0 Then
            enemy_block -= (b + strenght)
            Return 0
        Else
            enemy_block = 0
            Return Int(temp)
        End If
    End Function
    Function draw55() As Integer
        last = carduri_accesibile.Count - 1
        'MessageBox.Show(carduri_accesibile(last))
        card_system_9()
        PictureBox9.Visible = True
        draw_card()

        last = carduri_accesibile.Count - 1
        'MessageBox.Show(carduri_accesibile(last))
        card_system_10()
        PictureBox10.Visible = True
        draw_card()

        last = carduri_accesibile.Count - 1
        'MessageBox.Show(carduri_accesibile(last))
        card_system_11()
        PictureBox11.Visible = True
        draw_card()

        last = carduri_accesibile.Count - 1
        ' MessageBox.Show(carduri_accesibile(last))
        card_system_12()
        PictureBox12.Visible = True
        draw_card()

        last = carduri_accesibile.Count - 1
        'MessageBox.Show(carduri_accesibile(last))
        card_system_13()
        PictureBox13.Visible = True

        draw_card()
        last = carduri_accesibile.Count - 1
        updater()
    End Function
    Function draw1() As Integer
        last = carduri_accesibile.Count - 1
        'MessageBox.Show(carduri_accesibile(last))
        card_system_9()
        PictureBox9.Visible = True
        draw_card()
    End Function
    Function draw2() As Integer
        last = carduri_accesibile.Count - 1
        'MessageBox.Show(carduri_accesibile(last))
        card_system_10()
        PictureBox10.Visible = True
        draw_card()
    End Function
    Function draw3() As Integer
        last = carduri_accesibile.Count - 1
        'MessageBox.Show(carduri_accesibile(last))
        card_system_11()
        PictureBox11.Visible = True
        draw_card()
    End Function
    Function draw4() As Integer
        last = carduri_accesibile.Count - 1
        'MessageBox.Show(carduri_accesibile(last))
        card_system_12()
        PictureBox12.Visible = True
        draw_card()
    End Function
    Function draw5() As Integer
        last = carduri_accesibile.Count - 1
        'MessageBox.Show(carduri_accesibile(last))
        card_system_13()
        PictureBox13.Visible = True
        draw_card()
    End Function
    Function draw6() As Integer
        last = carduri_accesibile.Count - 1
        'MessageBox.Show(carduri_accesibile(last))
        card_system_14()
        PictureBox14.Visible = True
        draw_card()
    End Function
    Function draw7() As Integer
        last = carduri_accesibile.Count - 1
        'MessageBox.Show(carduri_accesibile(last))
        card_system_15()
        PictureBox15.Visible = True
        draw_card()
    End Function
    Function draw8() As Integer
        last = carduri_accesibile.Count - 1
        'MessageBox.Show(carduri_accesibile(last))
        card_system_16()
        PictureBox16.Visible = True
        draw_card()
    End Function
    Function draw9() As Integer
        last = carduri_accesibile.Count - 1
        'MessageBox.Show(carduri_accesibile(last))
        card_system_17()
        PictureBox17.Visible = True
        draw_card()
    End Function
    Function draw10() As Integer
        last = carduri_accesibile.Count - 1
        'MessageBox.Show(carduri_accesibile(last))
        card_system_18()
        PictureBox18.Visible = True
        draw_card()
    End Function
    Function draw11() As Integer
        last = carduri_accesibile.Count - 1
        'MessageBox.Show(carduri_accesibile(last))
        card_system_19()
        PictureBox19.Visible = True
        draw_card()
    End Function
    Private Sub Form1_load(ByVal sender As Object, ByVal e As EventArgs) Handles MyBase.Load

        PictureBox9.Image = Nothing
        PictureBox10.Image = Nothing
        PictureBox11.Image = Nothing
        PictureBox12.Image = Nothing
        PictureBox13.Image = Nothing
        PictureBox14.Image = Nothing
        PictureBox15.Image = Nothing
        PictureBox16.Image = Nothing
        PictureBox17.Image = Nothing
        PictureBox18.Image = Nothing
        PictureBox19.Image = Nothing

        startingdeck()

        deckupgrade()

        last = carduri_accesibile.Count - 1

        Shuffle(carduri_accesibile)
        cards_total = carduri_accesibile.Count
        Label8.Text = " " & cards_total

        draw1()
        draw2()
        draw3()
        draw4()
        draw5()
        'draw55()

        My.Computer.Audio.Play(My.Resources.Exordium,
          AudioPlayMode.BackgroundLoop)

        hp = 80
        hp_total = 80
        energy = 3
        totalenergy = 3
        floor = 1
        cards_discarded = 0
        hp_enemy = 32
        hp_enemy_total = 32

        strenght = 0
        block = 0
        contor_bloodletting = 0
        enemy_block = 0
        flex_contor = 0

        mesaj = "Alegeti numele dupa care vei ramana in istorie, golane."
        titlu = "Titlu inspirant si cliseic."
        primordial = "Mircea Badea spin"
        yname = InputBox(mesaj, titlu, primordial)

        vulnerable = True
        weak = True
        barricade = True
        battletrance = True
        berserk = True
        brutality = True
        demon_form = True
        combust = True
        flame_barrier = True

        ' energy
        Label1.Text = energy & "/" & totalenergy
        ' hp
        Label10.Text = hp & "/" & hp_total
        Label5.Text = hp & "/" & hp_total
        ' enemy hp
        Label9.Text = hp_enemy & "/" & hp_enemy_total
        ' cards in deck
        Label2.Text = carduri_accesibile.Count()
        ' cards discarded
        Label3.Text = cards_discarded
        ' total cards
        Label8.Text = cards_total
        ' floor 
        Label7.Text = floor
        ' enemy action
        Label11.Text = enemy_block
        ' your name
        Label4.Text = yname
        ' your block
        Label12.Text = block
        'your str
        Label14.Text = "Strenght: " & strenght
        ' enemy action
        enemy_action = random()
        If enemy_action = 1 Then
            Label13.Text = "6 dmg"
        ElseIf enemy_action = 2 Then
            Label13.Text = "12 block"
        ElseIf enemy_action = 3 Then
            Label13.Text = "9 block + 9 dmg"
        ElseIf enemy_action = 4 Then
            Label13.Text = "block 8 + remove debuff"
        ElseIf enemy_action = 5 Then
            Label13.Text = "25 dmg"
        ElseIf enemy_action = 6 Then
            Label13.Text = "Fumble"
        End If

    End Sub
    Private Sub card_system_9()
        Select Case carduri_accesibile(last)
            Case 0
                PictureBox9.Image = My.Resources.strike
            Case 1
                PictureBox9.Image = My.Resources.block
            Case 2
                PictureBox9.Image = My.Resources.Bash
            Case 3
                PictureBox9.Image = My.Resources.Clothesline
            Case 4
                PictureBox9.Image = My.Resources.Bloodletting
            Case 5
                PictureBox9.Image = My.Resources.Anger
            Case 6
                PictureBox9.Image = My.Resources.Barricade
            Case 7
                PictureBox9.Image = My.Resources.BattleTrance
            Case 8
                PictureBox9.Image = My.Resources.Berserk
            Case 9
                PictureBox9.Image = My.Resources.Bludgeon
            Case 10
                PictureBox9.Image = My.Resources.BodySlam
            Case 11
                PictureBox9.Image = My.Resources.Brutality
            Case 12
                PictureBox9.Image = My.Resources.DemonForm
            Case 13
                PictureBox9.Image = My.Resources.Combust
            Case 14
                PictureBox9.Image = My.Resources.Dropkick
            Case 15
                PictureBox9.Image = My.Resources.Dazed
            Case 16
                PictureBox9.Image = My.Resources.Wound
            Case 17
                PictureBox9.Image = My.Resources.Entrench
            Case 18
                PictureBox9.Image = My.Resources.FlameBarrier
            Case 19
                PictureBox9.Image = My.Resources.Flex
            Case 20
                PictureBox9.Image = My.Resources.PommelStrike
            Case 21
                PictureBox9.Image = My.Resources.Inflame
            Case 22
                PictureBox9.Image = My.Resources.RecklessCharge
            Case 23
                PictureBox9.Image = My.Resources.ShrugItOff
            Case 24
                PictureBox9.Image = My.Resources.WildStrike
            Case 25
                PictureBox9.Image = My.Resources.IronWave

        End Select
    End Sub
    Private Sub card_system_10()
        Select Case carduri_accesibile(last)
            Case 0
                PictureBox10.Image = My.Resources.strike
            Case 1
                PictureBox10.Image = My.Resources.block
            Case 2
                PictureBox10.Image = My.Resources.Bash
            Case 3
                PictureBox10.Image = My.Resources.Clothesline
            Case 4
                PictureBox10.Image = My.Resources.Bloodletting
            Case 5
                PictureBox10.Image = My.Resources.Anger
            Case 6
                PictureBox10.Image = My.Resources.Barricade
            Case 7
                PictureBox10.Image = My.Resources.BattleTrance
            Case 8
                PictureBox10.Image = My.Resources.Berserk
            Case 9
                PictureBox10.Image = My.Resources.Bludgeon
            Case 10
                PictureBox10.Image = My.Resources.BodySlam
            Case 11
                PictureBox10.Image = My.Resources.Brutality
            Case 12
                PictureBox10.Image = My.Resources.DemonForm
            Case 13
                PictureBox10.Image = My.Resources.Combust
            Case 14
                PictureBox10.Image = My.Resources.Dropkick
            Case 15
                PictureBox10.Image = My.Resources.Dazed
            Case 16
                PictureBox10.Image = My.Resources.Wound
            Case 17
                PictureBox10.Image = My.Resources.Entrench
            Case 18
                PictureBox10.Image = My.Resources.FlameBarrier
            Case 19
                PictureBox10.Image = My.Resources.Flex
            Case 20
                PictureBox10.Image = My.Resources.PommelStrike
            Case 21
                PictureBox10.Image = My.Resources.Inflame
            Case 22
                PictureBox10.Image = My.Resources.RecklessCharge
            Case 23
                PictureBox10.Image = My.Resources.ShrugItOff
            Case 24
                PictureBox10.Image = My.Resources.WildStrike
            Case 25
                PictureBox10.Image = My.Resources.IronWave

        End Select

    End Sub
    Private Sub card_system_11()
        Select Case carduri_accesibile(last)
            Case 0
                PictureBox11.Image = My.Resources.strike
            Case 1
                PictureBox11.Image = My.Resources.block
            Case 2
                PictureBox11.Image = My.Resources.Bash
            Case 3
                PictureBox11.Image = My.Resources.Clothesline
            Case 4
                PictureBox11.Image = My.Resources.Bloodletting
            Case 5
                PictureBox11.Image = My.Resources.Anger
            Case 6
                PictureBox11.Image = My.Resources.Barricade
            Case 7
                PictureBox11.Image = My.Resources.BattleTrance
            Case 8
                PictureBox11.Image = My.Resources.Berserk
            Case 9
                PictureBox11.Image = My.Resources.Bludgeon
            Case 10
                PictureBox11.Image = My.Resources.BodySlam
            Case 11
                PictureBox11.Image = My.Resources.Brutality
            Case 12
                PictureBox11.Image = My.Resources.DemonForm
            Case 13
                PictureBox11.Image = My.Resources.Combust
            Case 14
                PictureBox11.Image = My.Resources.Dropkick
            Case 15
                PictureBox11.Image = My.Resources.Dazed
            Case 16
                PictureBox11.Image = My.Resources.Wound
            Case 17
                PictureBox11.Image = My.Resources.Entrench
            Case 18
                PictureBox11.Image = My.Resources.FlameBarrier
            Case 19
                PictureBox11.Image = My.Resources.Flex
            Case 20
                PictureBox11.Image = My.Resources.PommelStrike
            Case 21
                PictureBox11.Image = My.Resources.Inflame
            Case 22
                PictureBox11.Image = My.Resources.RecklessCharge
            Case 23
                PictureBox11.Image = My.Resources.ShrugItOff
            Case 24
                PictureBox11.Image = My.Resources.WildStrike
            Case 25
                PictureBox11.Image = My.Resources.IronWave
        End Select
    End Sub
    Private Sub card_system_12()
        Select Case carduri_accesibile(last)
            Case 0
                PictureBox12.Image = My.Resources.strike
            Case 1
                PictureBox12.Image = My.Resources.block
            Case 2
                PictureBox12.Image = My.Resources.Bash
            Case 3
                PictureBox12.Image = My.Resources.Clothesline
            Case 4
                PictureBox12.Image = My.Resources.Bloodletting
            Case 5
                PictureBox12.Image = My.Resources.Anger
            Case 6
                PictureBox12.Image = My.Resources.Barricade
            Case 7
                PictureBox12.Image = My.Resources.BattleTrance
            Case 8
                PictureBox12.Image = My.Resources.Berserk
            Case 9
                PictureBox12.Image = My.Resources.Bludgeon
            Case 10
                PictureBox12.Image = My.Resources.BodySlam
            Case 11
                PictureBox12.Image = My.Resources.Brutality
            Case 12
                PictureBox12.Image = My.Resources.DemonForm
            Case 13
                PictureBox12.Image = My.Resources.Combust
            Case 14
                PictureBox12.Image = My.Resources.Dropkick
            Case 15
                PictureBox12.Image = My.Resources.Dazed
            Case 16
                PictureBox12.Image = My.Resources.Wound
            Case 17
                PictureBox12.Image = My.Resources.Entrench
            Case 18
                PictureBox12.Image = My.Resources.FlameBarrier
            Case 19
                PictureBox12.Image = My.Resources.Flex
            Case 20
                PictureBox12.Image = My.Resources.PommelStrike
            Case 21
                PictureBox12.Image = My.Resources.Inflame
            Case 22
                PictureBox12.Image = My.Resources.RecklessCharge
            Case 23
                PictureBox12.Image = My.Resources.ShrugItOff
            Case 24
                PictureBox12.Image = My.Resources.WildStrike
            Case 25
                PictureBox12.Image = My.Resources.IronWave
        End Select
    End Sub
    Private Sub card_system_13()
        Select Case carduri_accesibile(last)
            Case 0
                PictureBox13.Image = My.Resources.strike
            Case 1
                PictureBox13.Image = My.Resources.block
            Case 2
                PictureBox13.Image = My.Resources.Bash
            Case 3
                PictureBox13.Image = My.Resources.Clothesline
            Case 4
                PictureBox13.Image = My.Resources.Bloodletting
            Case 5
                PictureBox13.Image = My.Resources.Anger
            Case 6
                PictureBox13.Image = My.Resources.Barricade
            Case 7
                PictureBox13.Image = My.Resources.BattleTrance
            Case 8
                PictureBox13.Image = My.Resources.Berserk
            Case 9
                PictureBox13.Image = My.Resources.Bludgeon
            Case 10
                PictureBox13.Image = My.Resources.BodySlam
            Case 11
                PictureBox13.Image = My.Resources.Brutality
            Case 12
                PictureBox13.Image = My.Resources.DemonForm
            Case 13
                PictureBox13.Image = My.Resources.Combust
            Case 14
                PictureBox13.Image = My.Resources.Dropkick
            Case 15
                PictureBox13.Image = My.Resources.Dazed
            Case 16
                PictureBox13.Image = My.Resources.Wound
            Case 17
                PictureBox13.Image = My.Resources.Entrench
            Case 18
                PictureBox13.Image = My.Resources.FlameBarrier
            Case 19
                PictureBox13.Image = My.Resources.Flex
            Case 20
                PictureBox13.Image = My.Resources.PommelStrike
            Case 21
                PictureBox13.Image = My.Resources.Inflame
            Case 22
                PictureBox13.Image = My.Resources.RecklessCharge
            Case 23
                PictureBox13.Image = My.Resources.ShrugItOff
            Case 24
                PictureBox13.Image = My.Resources.WildStrike
            Case 25
                PictureBox13.Image = My.Resources.IronWave
        End Select
    End Sub
    Private Sub card_system_14()
        Select Case carduri_accesibile(last)
            Case 0
                PictureBox14.Image = My.Resources.strike
            Case 1
                PictureBox14.Image = My.Resources.block
            Case 2
                PictureBox14.Image = My.Resources.Bash
            Case 3
                PictureBox14.Image = My.Resources.Clothesline
            Case 4
                PictureBox14.Image = My.Resources.Bloodletting
            Case 5
                PictureBox14.Image = My.Resources.Anger
            Case 6
                PictureBox14.Image = My.Resources.Barricade
            Case 7
                PictureBox14.Image = My.Resources.BattleTrance
            Case 8
                PictureBox14.Image = My.Resources.Berserk
            Case 9
                PictureBox14.Image = My.Resources.Bludgeon
            Case 10
                PictureBox14.Image = My.Resources.BodySlam
            Case 11
                PictureBox14.Image = My.Resources.Brutality
            Case 12
                PictureBox14.Image = My.Resources.DemonForm
            Case 13
                PictureBox14.Image = My.Resources.Combust
            Case 14
                PictureBox14.Image = My.Resources.Dropkick
            Case 15
                PictureBox14.Image = My.Resources.Dazed
            Case 16
                PictureBox14.Image = My.Resources.Wound
            Case 17
                PictureBox14.Image = My.Resources.Entrench
            Case 18
                PictureBox14.Image = My.Resources.FlameBarrier
            Case 19
                PictureBox14.Image = My.Resources.Flex
            Case 20
                PictureBox14.Image = My.Resources.PommelStrike
            Case 21
                PictureBox14.Image = My.Resources.Inflame
            Case 22
                PictureBox14.Image = My.Resources.RecklessCharge
            Case 23
                PictureBox14.Image = My.Resources.ShrugItOff
            Case 24
                PictureBox14.Image = My.Resources.WildStrike
            Case 25
                PictureBox14.Image = My.Resources.IronWave
        End Select
    End Sub
    Private Sub card_system_15()
        Select Case carduri_accesibile(last)
            Case 0
                PictureBox15.Image = My.Resources.strike
            Case 1
                PictureBox15.Image = My.Resources.block
            Case 2
                PictureBox15.Image = My.Resources.Bash
            Case 3
                PictureBox15.Image = My.Resources.Clothesline
            Case 4
                PictureBox15.Image = My.Resources.Bloodletting
            Case 5
                PictureBox15.Image = My.Resources.Anger
            Case 6
                PictureBox15.Image = My.Resources.Barricade
            Case 7
                PictureBox15.Image = My.Resources.BattleTrance
            Case 8
                PictureBox15.Image = My.Resources.Berserk
            Case 9
                PictureBox15.Image = My.Resources.Bludgeon
            Case 10
                PictureBox15.Image = My.Resources.BodySlam
            Case 11
                PictureBox15.Image = My.Resources.Brutality
            Case 12
                PictureBox15.Image = My.Resources.DemonForm
            Case 13
                PictureBox15.Image = My.Resources.Combust
            Case 14
                PictureBox15.Image = My.Resources.Dropkick
            Case 15
                PictureBox15.Image = My.Resources.Dazed
            Case 16
                PictureBox15.Image = My.Resources.Wound
            Case 17
                PictureBox15.Image = My.Resources.Entrench
            Case 18
                PictureBox15.Image = My.Resources.FlameBarrier
            Case 19
                PictureBox15.Image = My.Resources.Flex
            Case 20
                PictureBox15.Image = My.Resources.PommelStrike
            Case 21
                PictureBox15.Image = My.Resources.Inflame
            Case 22
                PictureBox15.Image = My.Resources.RecklessCharge
            Case 23
                PictureBox15.Image = My.Resources.ShrugItOff
            Case 24
                PictureBox15.Image = My.Resources.WildStrike
            Case 25
                PictureBox15.Image = My.Resources.IronWave
        End Select
    End Sub
    Private Sub card_system_16()
        Select Case carduri_accesibile(last)
            Case 0
                PictureBox16.Image = My.Resources.strike
            Case 1
                PictureBox16.Image = My.Resources.block
            Case 2
                PictureBox16.Image = My.Resources.Bash
            Case 3
                PictureBox16.Image = My.Resources.Clothesline
            Case 4
                PictureBox16.Image = My.Resources.Bloodletting
            Case 5
                PictureBox16.Image = My.Resources.Anger
            Case 6
                PictureBox16.Image = My.Resources.Barricade
            Case 7
                PictureBox16.Image = My.Resources.BattleTrance
            Case 8
                PictureBox16.Image = My.Resources.Berserk
            Case 9
                PictureBox16.Image = My.Resources.Bludgeon
            Case 10
                PictureBox16.Image = My.Resources.BodySlam
            Case 11
                PictureBox16.Image = My.Resources.Brutality
            Case 12
                PictureBox16.Image = My.Resources.DemonForm
            Case 13
                PictureBox16.Image = My.Resources.Combust
            Case 14
                PictureBox16.Image = My.Resources.Dropkick
            Case 15
                PictureBox16.Image = My.Resources.Dazed
            Case 16
                PictureBox16.Image = My.Resources.Wound
            Case 17
                PictureBox16.Image = My.Resources.Entrench
            Case 18
                PictureBox16.Image = My.Resources.FlameBarrier
            Case 19
                PictureBox16.Image = My.Resources.Flex
            Case 20
                PictureBox16.Image = My.Resources.PommelStrike
            Case 21
                PictureBox16.Image = My.Resources.Inflame
            Case 22
                PictureBox16.Image = My.Resources.RecklessCharge
            Case 23
                PictureBox16.Image = My.Resources.ShrugItOff
            Case 24
                PictureBox16.Image = My.Resources.WildStrike
            Case 25
                PictureBox16.Image = My.Resources.IronWave
        End Select
    End Sub
    Private Sub card_system_17()
        Select Case carduri_accesibile(last)
            Case 0
                PictureBox17.Image = My.Resources.strike
            Case 1
                PictureBox17.Image = My.Resources.block
            Case 2
                PictureBox17.Image = My.Resources.Bash
            Case 3
                PictureBox17.Image = My.Resources.Clothesline
            Case 4
                PictureBox17.Image = My.Resources.Bloodletting
            Case 5
                PictureBox17.Image = My.Resources.Anger
            Case 6
                PictureBox17.Image = My.Resources.Barricade
            Case 7
                PictureBox17.Image = My.Resources.BattleTrance
            Case 8
                PictureBox17.Image = My.Resources.Berserk
            Case 9
                PictureBox17.Image = My.Resources.Bludgeon
            Case 10
                PictureBox17.Image = My.Resources.BodySlam
            Case 11
                PictureBox17.Image = My.Resources.Brutality
            Case 12
                PictureBox17.Image = My.Resources.DemonForm
            Case 13
                PictureBox17.Image = My.Resources.Combust
            Case 14
                PictureBox17.Image = My.Resources.Dropkick
            Case 15
                PictureBox17.Image = My.Resources.Dazed
            Case 16
                PictureBox17.Image = My.Resources.Wound
            Case 17
                PictureBox17.Image = My.Resources.Entrench
            Case 18
                PictureBox17.Image = My.Resources.FlameBarrier
            Case 19
                PictureBox17.Image = My.Resources.Flex
            Case 20
                PictureBox17.Image = My.Resources.PommelStrike
            Case 21
                PictureBox17.Image = My.Resources.Inflame
            Case 22
                PictureBox17.Image = My.Resources.RecklessCharge
            Case 23
                PictureBox17.Image = My.Resources.ShrugItOff
            Case 24
                PictureBox17.Image = My.Resources.WildStrike
            Case 25
                PictureBox17.Image = My.Resources.IronWave
        End Select
    End Sub
    Private Sub card_system_18()
        Select Case carduri_accesibile(last)
            Case 0
                PictureBox18.Image = My.Resources.strike
            Case 1
                PictureBox18.Image = My.Resources.block
            Case 2
                PictureBox18.Image = My.Resources.Bash
            Case 3
                PictureBox18.Image = My.Resources.Clothesline
            Case 4
                PictureBox18.Image = My.Resources.Bloodletting
            Case 5
                PictureBox18.Image = My.Resources.Anger
            Case 6
                PictureBox18.Image = My.Resources.Barricade
            Case 7
                PictureBox18.Image = My.Resources.BattleTrance
            Case 8
                PictureBox18.Image = My.Resources.Berserk
            Case 9
                PictureBox18.Image = My.Resources.Bludgeon
            Case 10
                PictureBox18.Image = My.Resources.BodySlam
            Case 11
                PictureBox18.Image = My.Resources.Brutality
            Case 12
                PictureBox18.Image = My.Resources.DemonForm
            Case 13
                PictureBox18.Image = My.Resources.Combust
            Case 14
                PictureBox18.Image = My.Resources.Dropkick
            Case 15
                PictureBox18.Image = My.Resources.Dazed
            Case 16
                PictureBox18.Image = My.Resources.Wound
            Case 17
                PictureBox18.Image = My.Resources.Entrench
            Case 18
                PictureBox18.Image = My.Resources.FlameBarrier
            Case 19
                PictureBox18.Image = My.Resources.Flex
            Case 20
                PictureBox18.Image = My.Resources.PommelStrike
            Case 21
                PictureBox18.Image = My.Resources.Inflame
            Case 22
                PictureBox18.Image = My.Resources.RecklessCharge
            Case 23
                PictureBox18.Image = My.Resources.ShrugItOff
            Case 24
                PictureBox18.Image = My.Resources.WildStrike
            Case 25
                PictureBox18.Image = My.Resources.IronWave
        End Select
    End Sub
    Private Sub card_system_19()
        Select Case carduri_accesibile(last)
            Case 0
                PictureBox19.Image = My.Resources.strike
            Case 1
                PictureBox19.Image = My.Resources.block
            Case 2
                PictureBox19.Image = My.Resources.Bash
            Case 3
                PictureBox19.Image = My.Resources.Clothesline
            Case 4
                PictureBox19.Image = My.Resources.Bloodletting
            Case 5
                PictureBox19.Image = My.Resources.Anger
            Case 6
                PictureBox19.Image = My.Resources.Barricade
            Case 7
                PictureBox19.Image = My.Resources.BattleTrance
            Case 8
                PictureBox19.Image = My.Resources.Berserk
            Case 9
                PictureBox19.Image = My.Resources.Bludgeon
            Case 10
                PictureBox19.Image = My.Resources.BodySlam
            Case 11
                PictureBox19.Image = My.Resources.Brutality
            Case 12
                PictureBox19.Image = My.Resources.DemonForm
            Case 13
                PictureBox19.Image = My.Resources.Combust
            Case 14
                PictureBox19.Image = My.Resources.Dropkick
            Case 15
                PictureBox19.Image = My.Resources.Dazed
            Case 16
                PictureBox19.Image = My.Resources.Wound
            Case 17
                PictureBox19.Image = My.Resources.Entrench
            Case 18
                PictureBox19.Image = My.Resources.FlameBarrier
            Case 19
                PictureBox19.Image = My.Resources.Flex
            Case 20
                PictureBox19.Image = My.Resources.PommelStrike
            Case 21
                PictureBox19.Image = My.Resources.Inflame
            Case 22
                PictureBox19.Image = My.Resources.RecklessCharge
            Case 23
                PictureBox19.Image = My.Resources.ShrugItOff
            Case 24
                PictureBox19.Image = My.Resources.WildStrike
            Case 25
                PictureBox19.Image = My.Resources.IronWave
        End Select
    End Sub
    Private Sub End_turn_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles End_turn.Click
        enemy_block = 0
        If hp_enemy <= 0 Then
            random_card()
            carduri_noi.Add(cox)
            MessageBox.Show("Ai primit cardul: " & cox)
            energy = 3
            floor += 1
            strenght = 0
            block = 0

            If hp <= hp_total - 6 Then
                hp += 6
            Else
                hp = hp_total
            End If

            flex_contor = 0
            contor_evulnerable = 0
            vulnerable = True
            contor_eweak = 0
            weak = True
            contor_bloodletting = 0
            barricade = True
            battletrance = True
            brutality = True
            demon_form = True
            combust = True

            enemy_block = 0

            hp_enemy = 25 + (5 * floor)
            hp_enemy_total = 25 + (5 * floor)

            enemy_action = random()

            carduri_functie.Clear()
            startingdeck()
            deckupgrade()
            Shuffle(carduri_accesibile)
            last = carduri_accesibile.Count - 1

            cards_total = carduri_accesibile.Count
            Label8.Text = " " & cards_total

            draw1()
            draw2()
            draw3()
            draw4()
            draw5()
            'draw55()
            updater()
            Randomize()
            Select Case Int((11 * Rnd()) + 1)
                Case 0
                    Enemy.Image = My.Resources.Acid_slime_l_pretty
                Case 1
                    Enemy.Image = My.Resources.Darkling
                Case 2
                    Enemy.Image = My.Resources.Giant_head_pretty
                Case 3
                    Enemy.Image = My.Resources.Jaw_worm_pretty
                Case 4
                    Enemy.Image = My.Resources.Looter_pretty
                Case 5
                    Enemy.Image = My.Resources.Louse_red_pretty
                Case 6
                    Enemy.Image = My.Resources.Sentry_pretty
                Case 7
                    Enemy.Image = My.Resources.Shelled_parasite_pretty
                Case 8
                    Enemy.Image = My.Resources.Slaver_blue_pretty
                Case 9
                    Enemy.Image = My.Resources.Slaver_red_pretty
                Case 10
                    Enemy.Image = My.Resources.Cultist_pretty
            End Select
        Else
            flame_barrier = False
            If weak Then
                If enemy_action = 1 Then
                    hp -= ydmg(block, 6)
                    If Not flame_barrier Then
                        hp_enemy -= 4
                    End If
                ElseIf enemy_action = 2 Then
                    enemy_block += 12
                ElseIf enemy_action = 3 Then
                    enemy_block = 9
                    hp -= ydmg(block, 9)
                    If Not flame_barrier Then
                        hp_enemy -= 4
                    End If
                ElseIf enemy_action = 4 Then
                    enemy_block = 8
                    vulnerable = True
                    contor_evulnerable = 0
                    weak = True
                    contor_eweak = 0
                ElseIf enemy_action = 5 Then
                    hp -= ydmg(block, 25)
                    If Not flame_barrier Then
                        hp_enemy -= 4
                    End If
                ElseIf enemy_action = 6 Then
                    hp_enemy -= 5
                    updater()
                ElseIf enemy_action = 7 Then
                    carduri_anger.Add(15)
                    carduri_anger.Add(15)
                    enemy_block += 8
                ElseIf enemy_action = 8 Then
                    carduri_anger.Add(16)
                    hp -= ydmg(block, 16)
                    If Not flame_barrier Then
                        hp_enemy -= 16
                    End If
                ElseIf enemy_action = 9 Then
                    hp_enemy += (15 / 100) * hp_enemy_total
                ElseIf enemy_action = 10 Then
                    strenght -= 2
                End If
            ElseIf weak = False Then
                If enemy_action = 1 Then
                    hp -= ydmg(block, 6 / 2)
                    If Not flame_barrier Then
                        hp_enemy -= 4
                    End If
                ElseIf enemy_action = 2 Then
                    enemy_block += 12
                ElseIf enemy_action = 3 Then
                    enemy_block = 9
                    hp -= ydmg(block, 9 / 2)
                    If Not flame_barrier Then
                        hp_enemy -= 4
                    End If
                ElseIf enemy_action = 4 Then
                    enemy_block = 8
                    vulnerable = True
                    contor_evulnerable = 0
                    weak = True
                    contor_eweak = 0
                ElseIf enemy_action = 5 Then
                    hp -= ydmg(block, 25 / 2)
                    If Not flame_barrier Then
                        hp_enemy -= 4
                    End If
                ElseIf enemy_action = 6 Then
                    hp_enemy -= Int(5 / 2)
                    updater()
                ElseIf enemy_action = 7 Then
                    carduri_anger.Add(15)
                    carduri_anger.Add(15)
                    enemy_block += 8
                ElseIf enemy_action = 8 Then
                    carduri_anger.Add(16)
                    hp -= ydmg(block, 16 / 2)
                    If Not flame_barrier Then
                        hp_enemy -= 16
                    End If
                ElseIf enemy_action = 9 Then
                    hp_enemy += (15 / 100) * hp_enemy_total
                ElseIf enemy_action = 10 Then
                    strenght -= 2
                End If
            End If
            updater()
            '/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
            If hp <= 0 Then
                Me.Close()
            End If

            flame_barrier = True
            If Not combust Then
                hp_enemy -= ydmg(enemy_block, 5)
                hp += -1
            End If
            If berserk Then
                energy = 3
                totalenergy = 3
            Else
                energy = 4
                totalenergy = 4
            End If

            If barricade Then
                block = 0
            Else
                block = block
            End If

            If Not demon_form Then
                strenght += 2
            End If

            If contor_eweak >= 1 Then
                contor_eweak += -1
            Else
                contor_eweak = 0
                weak = True
            End If
            If contor_evulnerable >= 1 Then
                contor_evulnerable += -1
            Else
                contor_evulnerable = 0
                vulnerable = True
            End If
            strenght -= flex_contor
            flex_contor = 0
            updater()
            enemy_action = random()
            If enemy_action = 1 Then
                Label13.Text = "6 dmg"
            ElseIf enemy_action = 2 Then
                Label13.Text = "12 block"
            ElseIf enemy_action = 3 Then
                Label13.Text = "9 block + 9 dmg"
            ElseIf enemy_action = 4 Then
                Label13.Text = "block 8 + remove debuff"
            ElseIf enemy_action = 5 Then
                Label13.Text = "25 dmg"
            ElseIf enemy_action = 6 Then
                Label13.Text = "Fumble"
            ElseIf enemy_action = 7 Then
                Label13.Text = "2x Dazed + 8 block"
            ElseIf enemy_action = 8 Then
                Label13.Text = "1x Wound + 4*4 dmg"
            ElseIf enemy_action = 9 Then
                Label13.Text = "heal self"
            ElseIf enemy_action = 10 Then
                Label13.Text = "Loose 2 strenght"
            End If
            If enemy_action = 1 And Not weak Then
                Label13.Text = "3 dmg"
            ElseIf enemy_action = 3 And Not weak Then
                Label13.Text = "9 block + 5 dmmg"
            ElseIf enemy_action = 5 And Not weak Then
                Label13.Text = "13 dmg"
            ElseIf enemy_action = 8 And Not weak Then
                Label13.Text = "1x Wound + 4*2 dmg"
            End If
            battletrance = True

            PictureBox9.Image = Nothing
            PictureBox10.Image = Nothing
            PictureBox11.Image = Nothing
            PictureBox12.Image = Nothing
            PictureBox13.Image = Nothing
            PictureBox14.Image = Nothing
            PictureBox15.Image = Nothing
            PictureBox16.Image = Nothing
            PictureBox17.Image = Nothing
            PictureBox18.Image = Nothing
            PictureBox19.Image = Nothing
            carduri_anger.Clear()
            carduri_functie.Clear()
            draw1()
            draw2()
            draw3()
            draw4()
            draw5()
            'draw55()
            If Not brutality Then
                hp += -1
                draw6()
            End If
            updater()

        End If
    End Sub
    '  ////////////////////////// CARDS /////////////////////////////
    Function card0() As Integer
        'CARD = STRIKE
        If energy > 0 Then
            energy -= 1
            If vulnerable Then
                hp_enemy -= ydmg(enemy_block, 6)
            Else
                hp_enemy -= ydmg(enemy_block, ((3 / 2) * 6))
            End If
            updater()
            Return 1
        Else
            shake()
            Return 0
        End If

    End Function
    Function card1() As Integer
        'CARD = BLOCK
        If energy > 0 Then
            energy -= 1
            block += 5
            updater()
            Return 1
        Else
            shake()
            Return 0
        End If
    End Function
    Function card2() As Integer
        'CARD = BASH
        If energy >= 2 Then
            energy -= 2
            If vulnerable Then
                hp_enemy -= ydmg(enemy_block, 8)
            Else
                hp_enemy -= ydmg(enemy_block, ((3 / 2) * 8))
            End If
            vulnerable = False
            contor_evulnerable = 2
            updater()
            Return 1
        Else
            shake()
            Return 0
        End If
    End Function
    Function card3() As Integer
        'CARD = CLOTHSLINE
        If energy >= 2 Then
            energy -= 2
            If vulnerable Then
                hp_enemy -= ydmg(enemy_block, 12)
            Else
                hp_enemy -= ydmg(enemy_block, ((3 / 2) * 12))
            End If
            weak = False
            contor_eweak = 2
            updater()
            Return 1
        Else
            shake()
            Return 0
        End If
    End Function
    Function card4() As Integer
        'CARD = BLOODLETTING
        If contor_bloodletting = 0 Then
            energy += 2
            hp -= 3
            contor_bloodletting += 1
            updater()
            Return 1
        ElseIf contor_bloodletting > 0 Then
            Return 0
        End If
    End Function
    Function card5() As Integer
        'card = anger
        If vulnerable Then
            hp_enemy -= ydmg(enemy_block, 6)
        Else
            hp_enemy -= ydmg(enemy_block, ((3 / 2) * 6))
        End If
        carduri_anger.Add(5)
        updater()
        Return 1
    End Function
    Function card6() As Integer
        'card = barricade
        If energy >= 3 Then
            energy += -3
            barricade = False
            updater()
            Return 1
        Else
            shake()
            Return 0
        End If
    End Function
    Function card7() As Integer
        'card = battletrance
        If battletrance = True Then
            draw_pointer()
            drawer()
            draw_pointer()
            drawer()
            draw_pointer()
            drawer()
            battletrance = False
            updater()
            Return 1
        Else
            shake()
            Return 0
        End If
    End Function
    Function card8() As Integer
        'card = berserk
        contor_yvulnerable += 2
        berserk = False
        updater()
        Return 1
    End Function
    Function card9() As Integer
        'CARD = bludgeon
        If energy >= 3 Then
            energy -= 3
            If vulnerable Then
                hp_enemy -= ydmg(enemy_block, 32)
            Else
                hp_enemy -= ydmg(enemy_block, ((3 / 2) * 32))
            End If
            updater()
            Return 1
        Else
            shake()
            Return 0
        End If
    End Function
    Function card10() As Integer
        'card = bodyslam
        If energy > 0 Then
            energy -= 1
            hp_enemy -= ydmg(enemy_block, block)
            updater()
            Return 1
        Else
            shake()
            Return 0
        End If
    End Function
    Function card11() As Integer
        'card = brutality
        brutality = False
        updater()
        Return 1
    End Function
    Function card12() As Integer
        'card = demon form
        If energy >= 3 Then
            energy += -3
            demon_form = False
            updater()
            Return 1
        Else
            shake()
            Return 0
        End If
    End Function
    Function card13() As Integer
        'card = combust
        If energy >= 1 Then
            combust = False
            updater()
            Return 1
        Else
            shake()
            Return 0
        End If
    End Function
    Function card_daze() As Integer
        shake()
        Return 0
    End Function
    Function card_wound() As Integer
        shake()
        Return 0
    End Function
    Function card14() As Integer
        'card = dropkick
        If energy >= 1 Then
            energy -= 1
            If vulnerable Then
                hp_enemy -= ydmg(enemy_block, 5)
            Else
                hp_enemy -= ydmg(enemy_block, ((3 / 2) * 5))
                draw_pointer()
                drawer()
                energy += 1
            End If
            updater()
            Return 1
        Else
            shake()
            Return 0
        End If

    End Function
    Function card15() As Integer
        'card = entrench
        If energy >= 2 Then
            energy -= 2
            block = block * 2
            updater()
            Return 1
        Else
            shake()
            Return 0
        End If
    End Function
    Function card16() As Integer
        'card = flame barrier
        If energy >= 2 Then
            energy -= 2
            block += 12
            updater()
            flame_barrier = False
            Return 1
        Else
            shake()
            Return 0
        End If
    End Function
    Function card17() As Integer
        'card = flex
        flex_contor += 2
        strenght += 2
        updater()
        Return 1
    End Function
    Function card18() As Integer
        'card = pommel strike
        If energy > 0 Then
            energy -= 1
            draw_pointer()
            drawer()
            If vulnerable Then
                hp_enemy -= ydmg(enemy_block, 9)
            Else
                hp_enemy -= ydmg(enemy_block, ((3 / 2) * 9))
            End If
            updater()
            Return 1
        Else
            shake()
            Return 0
        End If
    End Function
    Function card19() As Integer
        'card name = inflame
        If energy > 0 Then
            energy -= 1
            strenght += 2
            updater()
            Return 1
        Else
            shake()
            Return 0
        End If
    End Function
    Function card20() As Integer
        'card name = reckless charge
        If vulnerable Then
            hp_enemy -= ydmg(enemy_block, 7)
        Else
            hp_enemy -= ydmg(enemy_block, ((3 / 2) * 7))
        End If
        carduri_anger.Add(15)
        updater()
        Return 1
    End Function
    Function card21() As Integer
        'card name = shrug if off
        If energy > 0 Then
            energy -= 1
            block += 8
            draw_pointer()
            drawer()
            updater()
            Return 1
        Else
            shake()
            Return 0
        End If
    End Function
    Function card22() As Integer
        'card name = wild strike
        If energy > 0 Then
            energy -= 1
            If vulnerable Then
                hp_enemy -= ydmg(enemy_block, 12)
            Else
                hp_enemy -= ydmg(enemy_block, ((3 / 2) * 12))
            End If
            carduri_anger.Add(16)
            updater()
            Return 1
        Else
            shake()
            Return 0
        End If
    End Function
    Function card23() As Integer
        'card name = iron wave
        If energy > 0 Then
            energy -= 1
            If vulnerable Then
                hp_enemy -= ydmg(enemy_block, 5)
            Else
                hp_enemy -= ydmg(enemy_block, ((3 / 2) * 5))
            End If
            block += 5
            Return 1
        Else
            shake()
            Return 0
        End If
    End Function
    Function updater() As Integer
        Label1.Text = energy & "/" & totalenergy
        Label10.Text = hp & "/" & hp_total
        Label9.Text = hp_enemy & "/" & hp_enemy_total
        Label2.Text = carduri_accesibile.Count()
        Label3.Text = carduri_joc.Count()
        Label8.Text = cards_total
        Label11.Text = enemy_block
        Label12.Text = block
        Label14.Text = "Strenght: " & strenght
        Label5.Text = hp & "/" & hp_total
        yweak.Text = "Weak: " & contor_yweak
        yvulnerable.Text = "Vulnerable: " & contor_yvulnerable
        eweak.Text = "Weak: " & contor_eweak
        evulnerable.Text = "Vulnerable: " & contor_evulnerable
        Return 1
    End Function
    Private Sub PictureBox9_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox9.Click
        Select Case carduri_functie(0)
            Case 0
                If card0() = 1 Then
                    PictureBox9.Image = Nothing
                Else
                    PictureBox9.Visible = True
                End If

            Case 1
                If card1() = 1 Then
                    PictureBox9.Image = Nothing
                Else
                    PictureBox9.Visible = True
                End If
            Case 2
                If card2() = 1 Then
                    PictureBox9.Image = Nothing
                Else
                    PictureBox9.Visible = True
                End If
            Case 3
                If card3() = 1 Then
                    PictureBox9.Image = Nothing
                Else
                    PictureBox9.Visible = True
                End If
            Case 4
                If card4() = 1 Then
                    PictureBox9.Image = Nothing
                Else
                    PictureBox9.Visible = True
                End If
            Case 5
                If card5() = 1 Then
                    PictureBox9.Image = Nothing
                Else
                    PictureBox9.Visible = True
                End If
            Case 6
                If card6() = 1 Then
                    PictureBox9.Image = Nothing
                Else
                    PictureBox9.Visible = True
                End If
            Case 7
                If card7() = 1 Then
                    PictureBox9.Image = Nothing
                Else
                    PictureBox9.Visible = True
                End If
            Case 8
                If card8() = 1 Then
                    PictureBox9.Image = Nothing
                Else
                    PictureBox9.Visible = True
                End If
            Case 9
                If card9() = 1 Then
                    PictureBox9.Image = Nothing
                Else
                    PictureBox9.Visible = True
                End If
            Case 10
                If card10() = 1 Then
                    PictureBox9.Image = Nothing
                Else
                    PictureBox9.Visible = True
                End If
            Case 11
                If card11() = 1 Then
                    PictureBox9.Image = Nothing
                Else
                    PictureBox9.Visible = True
                End If
            Case 12
                If card12() = 1 Then
                    PictureBox9.Image = Nothing
                Else
                    PictureBox9.Visible = True
                End If
            Case 13
                If card13() = 1 Then
                    PictureBox9.Image = Nothing
                Else
                    PictureBox9.Visible = True
                End If
            Case 14
                If card14() = 1 Then
                    PictureBox9.Image = Nothing
                Else
                    PictureBox9.Visible = True
                End If
            Case 15
                If card_daze() = 1 Then
                    PictureBox9.Image = Nothing
                Else
                    PictureBox9.Visible = True
                End If
            Case 16
                If card_wound() = 1 Then
                    PictureBox9.Image = Nothing
                Else
                    PictureBox9.Visible = True
                End If
            Case 17
                If card15() = 1 Then
                    PictureBox9.Image = Nothing
                Else
                    PictureBox9.Visible = True
                End If
            Case 18
                If card16() = 1 Then
                    PictureBox9.Image = Nothing
                Else
                    PictureBox9.Visible = True
                End If
            Case 19
                If card17() = 1 Then
                    PictureBox9.Image = Nothing
                Else
                    PictureBox9.Visible = True
                End If
            Case 20
                If card18() = 1 Then
                    PictureBox9.Image = Nothing
                Else
                    PictureBox9.Visible = True
                End If
            Case 21
                If card19() = 1 Then
                    PictureBox9.Image = Nothing
                Else
                    PictureBox9.Visible = True
                End If
            Case 22
                If card20() = 1 Then
                    PictureBox9.Image = Nothing
                Else
                    PictureBox9.Visible = True
                End If
            Case 23
                If card21() = 1 Then
                    PictureBox9.Image = Nothing
                Else
                    PictureBox9.Visible = True
                End If
            Case 24
                If card22() = 1 Then
                    PictureBox9.Image = Nothing
                Else
                    PictureBox9.Visible = True
                End If
            Case 25
                If card23() = 1 Then
                    PictureBox9.Image = Nothing
                Else
                    PictureBox9.Visible = True
                End If
        End Select
    End Sub

    Private Sub PictureBox10_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox10.Click
        Select Case carduri_functie(1)
            Case 0
                If card0() = 1 Then
                    PictureBox10.Image = Nothing
                Else
                    PictureBox10.Visible = True
                End If
            Case 1
                If card1() = 1 Then
                    PictureBox10.Image = Nothing
                Else
                    PictureBox10.Visible = True
                End If
            Case 2
                If card2() = 1 Then
                    PictureBox10.Image = Nothing
                Else
                    PictureBox10.Visible = True
                End If
            Case 3
                If card3() = 1 Then
                    PictureBox10.Image = Nothing
                Else
                    PictureBox10.Visible = True
                End If
            Case 4
                If card4() = 1 Then
                    PictureBox10.Image = Nothing
                Else
                    PictureBox10.Visible = True
                End If
            Case 5
                If card5() = 1 Then
                    PictureBox10.Image = Nothing
                Else
                    PictureBox10.Visible = True
                End If
            Case 6
                If card6() = 1 Then
                    PictureBox10.Image = Nothing
                Else
                    PictureBox10.Visible = True
                End If
            Case 7
                If card7() = 1 Then
                    PictureBox10.Image = Nothing
                Else
                    PictureBox10.Visible = True
                End If
            Case 8
                If card8() = 1 Then
                    PictureBox10.Image = Nothing
                Else
                    PictureBox10.Visible = True
                End If
            Case 9
                If card9() = 1 Then
                    PictureBox10.Image = Nothing
                Else
                    PictureBox10.Visible = True
                End If
            Case 10
                If card10() = 1 Then
                    PictureBox10.Image = Nothing
                Else
                    PictureBox10.Visible = True
                End If
            Case 11
                If card11() = 1 Then
                    PictureBox10.Image = Nothing
                Else
                    PictureBox10.Visible = True
                End If
            Case 12
                If card12() = 1 Then
                    PictureBox10.Image = Nothing
                Else
                    PictureBox10.Visible = True
                End If
            Case 13
                If card13() = 1 Then
                    PictureBox10.Image = Nothing
                Else
                    PictureBox10.Visible = True
                End If
            Case 14
                If card14() = 1 Then
                    PictureBox10.Image = Nothing
                Else
                    PictureBox10.Visible = True
                End If
            Case 15
                If card_daze() = 1 Then
                    PictureBox10.Image = Nothing
                Else
                    PictureBox10.Visible = True
                End If
            Case 16
                If card_wound() = 1 Then
                    PictureBox10.Image = Nothing
                Else
                    PictureBox10.Visible = True
                End If
            Case 17
                If card15() = 1 Then
                    PictureBox10.Image = Nothing
                Else
                    PictureBox10.Visible = True
                End If
            Case 18
                If card16() = 1 Then
                    PictureBox10.Image = Nothing
                Else
                    PictureBox10.Visible = True
                End If
            Case 19
                If card17() = 1 Then
                    PictureBox10.Image = Nothing
                Else
                    PictureBox10.Visible = True
                End If
            Case 20
                If card18() = 1 Then
                    PictureBox10.Image = Nothing
                Else
                    PictureBox10.Visible = True
                End If
            Case 21
                If card19() = 1 Then
                    PictureBox10.Image = Nothing
                Else
                    PictureBox10.Visible = True
                End If
            Case 22
                If card20() = 1 Then
                    PictureBox10.Image = Nothing
                Else
                    PictureBox10.Visible = True
                End If
            Case 23
                If card21() = 1 Then
                    PictureBox10.Image = Nothing
                Else
                    PictureBox10.Visible = True
                End If
            Case 24
                If card22() = 1 Then
                    PictureBox10.Image = Nothing
                Else
                    PictureBox10.Visible = True
                End If
            Case 25
                If card23() = 1 Then
                    PictureBox10.Image = Nothing
                Else
                    PictureBox10.Visible = True
                End If
        End Select
    End Sub

    Private Sub PictureBox11_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox11.Click
        Select Case carduri_functie(2)
            Case 0
                If card0() = 1 Then
                    PictureBox11.Image = Nothing
                Else
                    PictureBox11.Visible = True
                End If
            Case 1
                If card1() = 1 Then
                    PictureBox11.Image = Nothing
                Else
                    PictureBox11.Visible = True
                End If
            Case 2
                If card2() = 1 Then
                    PictureBox11.Image = Nothing
                Else
                    PictureBox11.Visible = True
                End If
            Case 3
                If card3() = 1 Then
                    PictureBox11.Image = Nothing
                Else
                    PictureBox11.Visible = True
                End If
            Case 4
                If card4() = 1 Then
                    PictureBox11.Image = Nothing
                Else
                    PictureBox11.Visible = True
                End If
            Case 5
                If card5() = 1 Then
                    PictureBox11.Image = Nothing
                Else
                    PictureBox11.Visible = True
                End If
            Case 6
                If card6() = 1 Then
                    PictureBox11.Image = Nothing
                Else
                    PictureBox11.Visible = True
                End If
            Case 7
                If card7() = 1 Then
                    PictureBox11.Image = Nothing
                Else
                    PictureBox11.Visible = True
                End If
            Case 8
                If card8() = 1 Then
                    PictureBox11.Image = Nothing
                Else
                    PictureBox11.Visible = True
                End If
            Case 9
                If card9() = 1 Then
                    PictureBox11.Image = Nothing
                Else
                    PictureBox11.Visible = True
                End If
            Case 10
                If card10() = 1 Then
                    PictureBox11.Image = Nothing
                Else
                    PictureBox11.Visible = True
                End If
            Case 11
                If card11() = 1 Then
                    PictureBox11.Image = Nothing
                Else
                    PictureBox11.Visible = True
                End If
            Case 12
                If card12() = 1 Then
                    PictureBox11.Image = Nothing
                Else
                    PictureBox11.Visible = True
                End If
            Case 13
                If card13() = 1 Then
                    PictureBox11.Image = Nothing
                Else
                    PictureBox11.Visible = True
                End If
            Case 14
                If card14() = 1 Then
                    PictureBox11.Image = Nothing
                Else
                    PictureBox11.Visible = True
                End If
            Case 15
                If card_daze() = 1 Then
                    PictureBox11.Image = Nothing
                Else
                    PictureBox11.Visible = True
                End If
            Case 16
                If card_wound() = 1 Then
                    PictureBox11.Image = Nothing
                Else
                    PictureBox11.Visible = True
                End If
            Case 17
                If card15() = 1 Then
                    PictureBox11.Image = Nothing
                Else
                    PictureBox11.Visible = True
                End If
            Case 18
                If card16() = 1 Then
                    PictureBox11.Image = Nothing
                Else
                    PictureBox11.Visible = True
                End If
            Case 19
                If card17() = 1 Then
                    PictureBox11.Image = Nothing
                Else
                    PictureBox11.Visible = True
                End If
            Case 20
                If card18() = 1 Then
                    PictureBox11.Image = Nothing
                Else
                    PictureBox11.Visible = True
                End If
            Case 21
                If card19() = 1 Then
                    PictureBox11.Image = Nothing
                Else
                    PictureBox11.Visible = True
                End If
            Case 22
                If card20() = 1 Then
                    PictureBox11.Image = Nothing
                Else
                    PictureBox11.Visible = True
                End If
            Case 23
                If card21() = 1 Then
                    PictureBox11.Image = Nothing
                Else
                    PictureBox11.Visible = True
                End If
            Case 24
                If card22() = 1 Then
                    PictureBox11.Image = Nothing
                Else
                    PictureBox11.Visible = True
                End If
            Case 25
                If card23() = 1 Then
                    PictureBox11.Image = Nothing
                Else
                    PictureBox11.Visible = True
                End If
        End Select
    End Sub

    Private Sub PictureBox12_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox12.Click
        Select Case carduri_functie(3)
            Case 0
                If card0() = 1 Then
                    PictureBox12.Image = Nothing
                Else
                    PictureBox12.Visible = True
                End If
            Case 1
                If card1() = 1 Then
                    PictureBox12.Image = Nothing
                Else
                    PictureBox12.Visible = True
                End If
            Case 2
                If card2() = 1 Then
                    PictureBox12.Image = Nothing
                Else
                    PictureBox12.Visible = True
                End If
            Case 3
                If card3() = 1 Then
                    PictureBox12.Image = Nothing
                Else
                    PictureBox12.Visible = True
                End If
            Case 4
                If card4() = 1 Then
                    PictureBox12.Image = Nothing
                Else
                    PictureBox12.Visible = True
                End If
            Case 5
                If card5() = 1 Then
                    PictureBox12.Image = Nothing
                Else
                    PictureBox12.Visible = True
                End If
            Case 6
                If card6() = 1 Then
                    PictureBox12.Image = Nothing
                Else
                    PictureBox12.Visible = True
                End If
            Case 7
                If card7() = 1 Then
                    PictureBox12.Image = Nothing
                Else
                    PictureBox12.Visible = True
                End If
            Case 8
                If card8() = 1 Then
                    PictureBox12.Image = Nothing
                Else
                    PictureBox12.Visible = True
                End If
            Case 9
                If card9() = 1 Then
                    PictureBox12.Image = Nothing
                Else
                    PictureBox12.Visible = True
                End If
            Case 10
                If card10() = 1 Then
                    PictureBox12.Image = Nothing
                Else
                    PictureBox12.Visible = True
                End If
            Case 11
                If card11() = 1 Then
                    PictureBox12.Image = Nothing
                Else
                    PictureBox12.Visible = True
                End If
            Case 12
                If card12() = 1 Then
                    PictureBox12.Image = Nothing
                Else
                    PictureBox12.Visible = True
                End If
            Case 13
                If card13() = 1 Then
                    PictureBox12.Image = Nothing
                Else
                    PictureBox12.Visible = True
                End If
            Case 14
                If card14() = 1 Then
                    PictureBox12.Image = Nothing
                Else
                    PictureBox12.Visible = True
                End If
            Case 15
                If card_daze() = 1 Then
                    PictureBox12.Image = Nothing
                Else
                    PictureBox12.Visible = True
                End If
            Case 16
                If card_wound() = 1 Then
                    PictureBox12.Image = Nothing
                Else
                    PictureBox12.Visible = True
                End If
            Case 17
                If card15() = 1 Then
                    PictureBox12.Image = Nothing
                Else
                    PictureBox12.Visible = True
                End If
            Case 18
                If card16() = 1 Then
                    PictureBox12.Image = Nothing
                Else
                    PictureBox12.Visible = True
                End If
            Case 19
                If card17() = 1 Then
                    PictureBox12.Image = Nothing
                Else
                    PictureBox12.Visible = True
                End If
            Case 20
                If card18() = 1 Then
                    PictureBox12.Image = Nothing
                Else
                    PictureBox12.Visible = True
                End If
            Case 21
                If card19() = 1 Then
                    PictureBox12.Image = Nothing
                Else
                    PictureBox12.Visible = True
                End If
            Case 22
                If card20() = 1 Then
                    PictureBox12.Image = Nothing
                Else
                    PictureBox12.Visible = True
                End If
            Case 23
                If card21() = 1 Then
                    PictureBox12.Image = Nothing
                Else
                    PictureBox12.Visible = True
                End If
            Case 24
                If card22() = 1 Then
                    PictureBox12.Image = Nothing
                Else
                    PictureBox12.Visible = True
                End If
            Case 25
                If card23() = 1 Then
                    PictureBox12.Image = Nothing
                Else
                    PictureBox12.Visible = True
                End If
        End Select
    End Sub

    Private Sub PictureBox13_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox13.Click
        Select Case carduri_functie(4)
            Case 0
                If card0() = 1 Then
                    PictureBox13.Image = Nothing
                Else
                    PictureBox13.Visible = True
                End If
            Case 1
                If card1() = 1 Then
                    PictureBox13.Image = Nothing
                Else
                    PictureBox13.Visible = True
                End If
            Case 2
                If card2() = 1 Then
                    PictureBox13.Image = Nothing
                Else
                    PictureBox13.Visible = True
                End If
            Case 3
                If card3() = 1 Then
                    PictureBox13.Image = Nothing
                Else
                    PictureBox13.Visible = True
                End If
            Case 4
                If card4() = 1 Then
                    PictureBox13.Image = Nothing
                Else
                    PictureBox13.Visible = True
                End If
            Case 5
                If card5() = 1 Then
                    PictureBox13.Image = Nothing
                Else
                    PictureBox13.Visible = True
                End If
            Case 6
                If card6() = 1 Then
                    PictureBox13.Image = Nothing
                Else
                    PictureBox13.Visible = True
                End If
            Case 7
                If card7() = 1 Then
                    PictureBox13.Image = Nothing
                Else
                    PictureBox13.Visible = True
                End If
            Case 8
                If card8() = 1 Then
                    PictureBox13.Image = Nothing
                Else
                    PictureBox13.Visible = True
                End If
            Case 9
                If card9() = 1 Then
                    PictureBox13.Image = Nothing
                Else
                    PictureBox13.Visible = True
                End If
            Case 10
                If card10() = 1 Then
                    PictureBox13.Image = Nothing
                    Else
                        PictureBox13.Visible = True
                End If
            Case 11
                If card11() = 1 Then
                    PictureBox13.Image = Nothing
                Else
                    PictureBox13.Visible = True
                End If
            Case 12
                If card12() = 1 Then
                    PictureBox13.Image = Nothing
                Else
                    PictureBox13.Visible = True
                End If
            Case 13
                If card13() = 1 Then
                    PictureBox13.Image = Nothing
                Else
                    PictureBox13.Visible = True
                End If
            Case 14
                If card14() = 1 Then
                    PictureBox13.Image = Nothing
                Else
                    PictureBox13.Visible = True
                End If
            Case 15
                If card_daze() = 1 Then
                    PictureBox13.Image = Nothing
                Else
                    PictureBox13.Visible = True
                End If
            Case 16
                If card_wound() = 1 Then
                    PictureBox13.Image = Nothing
                Else
                    PictureBox13.Visible = True
                End If
            Case 17
                If card15() = 1 Then
                    PictureBox13.Image = Nothing
                Else
                    PictureBox13.Visible = True
                End If
            Case 18
                If card16() = 1 Then
                    PictureBox13.Image = Nothing
                Else
                    PictureBox13.Visible = True
                End If
            Case 19
                If card17() = 1 Then
                    PictureBox13.Image = Nothing
                Else
                    PictureBox13.Visible = True
                End If
            Case 20
                If card18() = 1 Then
                    PictureBox13.Image = Nothing
                Else
                    PictureBox13.Visible = True
                End If
            Case 21
                If card19() = 1 Then
                    PictureBox13.Image = Nothing
                Else
                    PictureBox13.Visible = True
                End If
            Case 22
                If card20() = 1 Then
                    PictureBox13.Image = Nothing
                Else
                    PictureBox13.Visible = True
                End If
            Case 23
                If card21() = 1 Then
                    PictureBox13.Image = Nothing
                Else
                    PictureBox13.Visible = True
                End If
            Case 24
                If card22() = 1 Then
                    PictureBox13.Image = Nothing
                Else
                    PictureBox13.Visible = True
                End If
            Case 25
                If card23() = 1 Then
                    PictureBox13.Image = Nothing
                Else
                    PictureBox13.Visible = True
                End If
        End Select
    End Sub

    Private Sub PictureBox14_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox14.Click
        Select Case carduri_functie(5)
            Case 0
                If card0() = 1 Then
                    PictureBox14.Image = Nothing
                Else
                    PictureBox14.Visible = True
                End If
            Case 1
                If card1() = 1 Then
                    PictureBox14.Image = Nothing
                Else
                    PictureBox14.Visible = True
                End If
            Case 2
                If card2() = 1 Then
                    PictureBox14.Image = Nothing
                Else
                    PictureBox14.Visible = True
                End If
            Case 3
                If card3() = 1 Then
                    PictureBox14.Image = Nothing
                Else
                    PictureBox14.Visible = True
                End If
            Case 4
                If card4() = 1 Then
                    PictureBox14.Image = Nothing
                Else
                    PictureBox14.Visible = True
                End If
            Case 5
                If card5() = 1 Then
                    PictureBox14.Image = Nothing
                Else
                    PictureBox14.Visible = True
                End If
            Case 6
                If card6() = 1 Then
                    PictureBox14.Image = Nothing
                Else
                    PictureBox14.Visible = True
                End If
            Case 7
                If card7() = 1 Then
                    PictureBox14.Image = Nothing
                Else
                    PictureBox14.Visible = True
                End If
            Case 8
                If card8() = 1 Then
                    PictureBox14.Image = Nothing
                Else
                    PictureBox14.Visible = True
                End If
            Case 9
                If card9() = 1 Then
                    PictureBox14.Image = Nothing
                Else
                    PictureBox14.Visible = True
                End If
            Case 10
                If card10() = 1 Then
                    PictureBox14.Image = Nothing
                Else
                    PictureBox14.Visible = True
                End If
            Case 11
                If card11() = 1 Then
                    PictureBox14.Image = Nothing
                Else
                    PictureBox14.Visible = True
                End If
            Case 12
                If card12() = 1 Then
                    PictureBox14.Image = Nothing
                Else
                    PictureBox14.Visible = True
                End If
            Case 13
                If card13() = 1 Then
                    PictureBox14.Image = Nothing
                Else
                    PictureBox14.Visible = True
                End If
            Case 14
                If card14() = 1 Then
                    PictureBox14.Image = Nothing
                Else
                    PictureBox14.Visible = True
                End If
            Case 15
                If card_daze() = 1 Then
                    PictureBox14.Image = Nothing
                Else
                    PictureBox14.Visible = True
                End If
            Case 16
                If card_wound() = 1 Then
                    PictureBox14.Image = Nothing
                Else
                    PictureBox14.Visible = True
                End If
            Case 17
                If card15() = 1 Then
                    PictureBox14.Image = Nothing
                Else
                    PictureBox14.Visible = True
                End If
            Case 18
                If card16() = 1 Then
                    PictureBox14.Image = Nothing
                Else
                    PictureBox14.Visible = True
                End If
            Case 19
                If card17() = 1 Then
                    PictureBox14.Image = Nothing
                Else
                    PictureBox14.Visible = True
                End If
            Case 20
                If card18() = 1 Then
                    PictureBox14.Image = Nothing
                Else
                    PictureBox14.Visible = True
                End If
            Case 21
                If card19() = 1 Then
                    PictureBox14.Image = Nothing
                Else
                    PictureBox14.Visible = True
                End If
            Case 22
                If card20() = 1 Then
                    PictureBox14.Image = Nothing
                Else
                    PictureBox14.Visible = True
                End If
            Case 23
                If card21() = 1 Then
                    PictureBox14.Image = Nothing
                Else
                    PictureBox14.Visible = True
                End If
            Case 24
                If card22() = 1 Then
                    PictureBox14.Image = Nothing
                Else
                    PictureBox14.Visible = True
                End If
            Case 25
                If card23() = 1 Then
                    PictureBox14.Image = Nothing
                Else
                    PictureBox14.Visible = True
                End If
        End Select
    End Sub

    Private Sub PictureBox15_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox15.Click
        Select Case carduri_functie(6)
            Case 0
                If card0() = 1 Then
                    PictureBox15.Image = Nothing
                Else
                    PictureBox15.Visible = True
                End If
            Case 1
                If card1() = 1 Then
                    PictureBox15.Image = Nothing
                Else
                    PictureBox15.Visible = True
                End If
            Case 2
                If card2() = 1 Then
                    PictureBox15.Image = Nothing
                Else
                    PictureBox15.Visible = True
                End If
            Case 3
                If card3() = 1 Then
                    PictureBox15.Image = Nothing
                Else
                    PictureBox15.Visible = True
                End If
            Case 4
                If card4() = 1 Then
                    PictureBox15.Image = Nothing
                Else
                    PictureBox15.Visible = True
                End If
            Case 5
                If card5() = 1 Then
                    PictureBox15.Image = Nothing
                Else
                    PictureBox15.Visible = True
                End If
            Case 6
                If card6() = 1 Then
                    PictureBox15.Image = Nothing
                Else
                    PictureBox15.Visible = True
                End If
            Case 7
                If card7() = 1 Then
                    PictureBox15.Image = Nothing
                Else
                    PictureBox15.Visible = True
                End If
            Case 8
                If card8() = 1 Then
                    PictureBox15.Image = Nothing
                Else
                    PictureBox15.Visible = True
                End If
            Case 9
                If card9() = 1 Then
                    PictureBox15.Image = Nothing
                Else
                    PictureBox15.Visible = True
                End If
            Case 10
                If card10() = 1 Then
                    PictureBox15.Image = Nothing
                Else
                    PictureBox15.Visible = True
                End If
            Case 11
                If card11() = 1 Then
                    PictureBox15.Image = Nothing
                Else
                    PictureBox15.Visible = True
                End If
            Case 12
                If card12() = 1 Then
                    PictureBox15.Image = Nothing
                Else
                    PictureBox15.Visible = True
                End If
            Case 13
                If card13() = 1 Then
                    PictureBox15.Image = Nothing
                Else
                    PictureBox15.Visible = True
                End If
            Case 14
                If card14() = 1 Then
                    PictureBox15.Image = Nothing
                Else
                    PictureBox15.Visible = True
                End If
            Case 15
                If card_daze() = 1 Then
                    PictureBox15.Image = Nothing
                Else
                    PictureBox15.Visible = True
                End If
            Case 16
                If card_wound() = 1 Then
                    PictureBox15.Image = Nothing
                Else
                    PictureBox15.Visible = True
                End If
            Case 17
                If card15() = 1 Then
                    PictureBox15.Image = Nothing
                Else
                    PictureBox15.Visible = True
                End If
            Case 18
                If card16() = 1 Then
                    PictureBox15.Image = Nothing
                Else
                    PictureBox15.Visible = True
                End If
            Case 19
                If card17() = 1 Then
                    PictureBox15.Image = Nothing
                Else
                    PictureBox15.Visible = True
                End If
            Case 20
                If card18() = 1 Then
                    PictureBox15.Image = Nothing
                Else
                    PictureBox15.Visible = True
                End If
            Case 21
                If card19() = 1 Then
                    PictureBox15.Image = Nothing
                Else
                    PictureBox15.Visible = True
                End If
            Case 22
                If card20() = 1 Then
                    PictureBox15.Image = Nothing
                Else
                    PictureBox15.Visible = True
                End If
            Case 23
                If card21() = 1 Then
                    PictureBox15.Image = Nothing
                Else
                    PictureBox15.Visible = True
                End If
            Case 24
                If card22() = 1 Then
                    PictureBox15.Image = Nothing
                Else
                    PictureBox15.Visible = True
                End If
            Case 25
                If card23() = 1 Then
                    PictureBox15.Image = Nothing
                Else
                    PictureBox15.Visible = True
                End If
        End Select
    End Sub

    Private Sub PictureBox16_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox16.Click
        Select Case carduri_functie(7)
            Case 0
                If card0() = 1 Then
                    PictureBox16.Image = Nothing
                Else
                    PictureBox16.Visible = True
                End If
            Case 1
                If card1() = 1 Then
                    PictureBox16.Image = Nothing
                Else
                    PictureBox16.Visible = True
                End If
            Case 2
                If card2() = 1 Then
                    PictureBox16.Image = Nothing
                Else
                    PictureBox16.Visible = True
                End If
            Case 3
                If card3() = 1 Then
                    PictureBox16.Image = Nothing
                Else
                    PictureBox16.Visible = True
                End If
            Case 4
                If card4() = 1 Then
                    PictureBox16.Image = Nothing
                Else
                    PictureBox16.Visible = True
                End If
            Case 5
                If card5() = 1 Then
                    PictureBox16.Image = Nothing
                Else
                    PictureBox16.Visible = True
                End If
            Case 6
                If card6() = 1 Then
                    PictureBox16.Image = Nothing
                Else
                    PictureBox16.Visible = True
                End If
            Case 7
                If card7() = 1 Then
                    PictureBox16.Image = Nothing
                Else
                    PictureBox16.Visible = True
                End If
            Case 8
                If card8() = 1 Then
                    PictureBox16.Image = Nothing
                Else
                    PictureBox16.Visible = True
                End If
            Case 9
                If card9() = 1 Then
                    PictureBox16.Image = Nothing
                Else
                    PictureBox16.Visible = True
                End If
            Case 10
                If card10() = 1 Then
                    PictureBox16.Image = Nothing
                Else
                    PictureBox16.Visible = True
                End If
            Case 11
                If card11() = 1 Then
                    PictureBox16.Image = Nothing
                Else
                    PictureBox16.Visible = True
                End If
            Case 12
                If card12() = 1 Then
                    PictureBox16.Image = Nothing
                Else
                    PictureBox16.Visible = True
                End If
            Case 13
                If card13() = 1 Then
                    PictureBox16.Image = Nothing
                Else
                    PictureBox16.Visible = True
                End If
            Case 14
                If card14() = 1 Then
                    PictureBox16.Image = Nothing
                Else
                    PictureBox16.Visible = True
                End If
            Case 15
                If card_daze() = 1 Then
                    PictureBox16.Image = Nothing
                Else
                    PictureBox16.Visible = True
                End If
            Case 16
                If card_wound() = 1 Then
                    PictureBox16.Image = Nothing
                Else
                    PictureBox16.Visible = True
                End If
            Case 17
                If card15() = 1 Then
                    PictureBox16.Image = Nothing
                Else
                    PictureBox16.Visible = True
                End If
            Case 18
                If card16() = 1 Then
                    PictureBox16.Image = Nothing
                Else
                    PictureBox16.Visible = True
                End If
            Case 19
                If card17() = 1 Then
                    PictureBox16.Image = Nothing
                Else
                    PictureBox16.Visible = True
                End If
            Case 20
                If card18() = 1 Then
                    PictureBox16.Image = Nothing
                Else
                    PictureBox16.Visible = True
                End If
            Case 21
                If card19() = 1 Then
                    PictureBox16.Image = Nothing
                Else
                    PictureBox16.Visible = True
                End If
            Case 22
                If card20() = 1 Then
                    PictureBox16.Image = Nothing
                Else
                    PictureBox16.Visible = True
                End If
            Case 23
                If card21() = 1 Then
                    PictureBox16.Image = Nothing
                Else
                    PictureBox16.Visible = True
                End If
            Case 24
                If card22() = 1 Then
                    PictureBox16.Image = Nothing
                Else
                    PictureBox16.Visible = True
                End If
            Case 25
                If card23() = 1 Then
                    PictureBox16.Image = Nothing
                Else
                    PictureBox16.Visible = True
                End If
        End Select
    End Sub

    Private Sub PictureBox17_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox17.Click
        Select Case carduri_functie(8)
            Case 0
                If card0() = 1 Then
                    PictureBox17.Image = Nothing
                Else
                    PictureBox17.Visible = True
                End If
            Case 1
                If card1() = 1 Then
                    PictureBox17.Image = Nothing
                Else
                    PictureBox17.Visible = True
                End If
            Case 2
                If card2() = 1 Then
                    PictureBox17.Image = Nothing
                Else
                    PictureBox17.Visible = True
                End If
            Case 3
                If card3() = 1 Then
                    PictureBox17.Image = Nothing
                Else
                    PictureBox17.Visible = True
                End If
            Case 4
                If card4() = 1 Then
                    PictureBox17.Image = Nothing
                Else
                    PictureBox17.Visible = True
                End If
            Case 5
                If card5() = 1 Then
                    PictureBox17.Image = Nothing
                Else
                    PictureBox17.Visible = True
                End If
            Case 6
                If card6() = 1 Then
                    PictureBox17.Image = Nothing
                Else
                    PictureBox17.Visible = True
                End If
            Case 7
                If card7() = 1 Then
                    PictureBox17.Image = Nothing
                Else
                    PictureBox17.Visible = True
                End If
            Case 8
                If card8() = 1 Then
                    PictureBox17.Image = Nothing
                Else
                    PictureBox17.Visible = True
                End If
            Case 9
                If card9() = 1 Then
                    PictureBox17.Image = Nothing
                Else
                    PictureBox17.Visible = True
                End If
            Case 10
                If card10() = 1 Then
                    PictureBox17.Image = Nothing
                Else
                    PictureBox17.Visible = True
                End If
            Case 11
                If card11() = 1 Then
                    PictureBox17.Image = Nothing
                Else
                    PictureBox17.Visible = True
                End If
            Case 12
                If card12() = 1 Then
                    PictureBox17.Image = Nothing
                Else
                    PictureBox17.Visible = True
                End If
            Case 13
                If card13() = 1 Then
                    PictureBox17.Image = Nothing
                Else
                    PictureBox17.Visible = True
                End If
            Case 14
                If card14() = 1 Then
                    PictureBox17.Image = Nothing
                Else
                    PictureBox17.Visible = True
                End If
            Case 15
                If card_daze() = 1 Then
                    PictureBox17.Image = Nothing
                Else
                    PictureBox17.Visible = True
                End If
            Case 16
                If card_wound() = 1 Then
                    PictureBox17.Image = Nothing
                Else
                    PictureBox17.Visible = True
                End If
            Case 17
                If card15() = 1 Then
                    PictureBox17.Image = Nothing
                Else
                    PictureBox17.Visible = True
                End If
            Case 18
                If card16() = 1 Then
                    PictureBox17.Image = Nothing
                Else
                    PictureBox17.Visible = True
                End If
            Case 19
                If card17() = 1 Then
                    PictureBox17.Image = Nothing
                Else
                    PictureBox17.Visible = True
                End If
            Case 20
                If card18() = 1 Then
                    PictureBox17.Image = Nothing
                Else
                    PictureBox17.Visible = True
                End If
            Case 21
                If card19() = 1 Then
                    PictureBox17.Image = Nothing
                Else
                    PictureBox17.Visible = True
                End If
            Case 22
                If card20() = 1 Then
                    PictureBox17.Image = Nothing
                Else
                    PictureBox17.Visible = True
                End If
            Case 23
                If card21() = 1 Then
                    PictureBox17.Image = Nothing
                Else
                    PictureBox17.Visible = True
                End If
            Case 24
                If card22() = 1 Then
                    PictureBox17.Image = Nothing
                Else
                    PictureBox17.Visible = True
                End If
            Case 25
                If card23() = 1 Then
                    PictureBox17.Image = Nothing
                Else
                    PictureBox17.Visible = True
                End If
        End Select
    End Sub

    Private Sub PictureBox18_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox18.Click
        Select Case carduri_functie(9)
            Case 0
                If card0() = 1 Then
                    PictureBox18.Image = Nothing
                Else
                    PictureBox18.Visible = True
                End If
            Case 1
                If card1() = 1 Then
                    PictureBox18.Image = Nothing
                Else
                    PictureBox18.Visible = True
                End If
            Case 2
                If card2() = 1 Then
                    PictureBox18.Image = Nothing
                Else
                    PictureBox18.Visible = True
                End If
            Case 3
                If card3() = 1 Then
                    PictureBox18.Image = Nothing
                Else
                    PictureBox18.Visible = True
                End If
            Case 4
                If card4() = 1 Then
                    PictureBox18.Image = Nothing
                Else
                    PictureBox18.Visible = True
                End If
            Case 5
                If card5() = 1 Then
                    PictureBox18.Image = Nothing
                Else
                    PictureBox18.Visible = True
                End If
            Case 6
                If card6() = 1 Then
                    PictureBox18.Image = Nothing
                Else
                    PictureBox18.Visible = True
                End If
            Case 7
                If card7() = 1 Then
                    PictureBox18.Image = Nothing
                Else
                    PictureBox18.Visible = True
                End If
            Case 8
                If card8() = 1 Then
                    PictureBox18.Image = Nothing
                Else
                    PictureBox18.Visible = True
                End If
            Case 9
                If card9() = 1 Then
                    PictureBox18.Image = Nothing
                Else
                    PictureBox18.Visible = True
                End If
            Case 10
                If card10() = 1 Then
                    PictureBox18.Image = Nothing
                Else
                    PictureBox18.Visible = True
                End If
            Case 11
                If card11() = 1 Then
                    PictureBox18.Image = Nothing
                Else
                    PictureBox18.Visible = True
                End If
            Case 12
                If card12() = 1 Then
                    PictureBox18.Image = Nothing
                Else
                    PictureBox18.Visible = True
                End If
            Case 13
                If card13() = 1 Then
                    PictureBox18.Image = Nothing
                Else
                    PictureBox18.Visible = True
                End If
            Case 14
                If card14() = 1 Then
                    PictureBox18.Image = Nothing
                Else
                    PictureBox18.Visible = True
                End If
            Case 15
                If card_daze() = 1 Then
                    PictureBox18.Image = Nothing
                Else
                    PictureBox18.Visible = True
                End If
            Case 16
                If card_wound() = 1 Then
                    PictureBox18.Image = Nothing
                Else
                    PictureBox18.Visible = True
                End If
            Case 17
                If card15() = 1 Then
                    PictureBox18.Image = Nothing
                Else
                    PictureBox18.Visible = True
                End If
            Case 18
                If card16() = 1 Then
                    PictureBox18.Image = Nothing
                Else
                    PictureBox18.Visible = True
                End If
            Case 19
                If card17() = 1 Then
                    PictureBox18.Image = Nothing
                Else
                    PictureBox18.Visible = True
                End If
            Case 20
                If card18() = 1 Then
                    PictureBox18.Image = Nothing
                Else
                    PictureBox18.Visible = True
                End If
            Case 21
                If card19() = 1 Then
                    PictureBox18.Image = Nothing
                Else
                    PictureBox18.Visible = True
                End If
            Case 22
                If card20() = 1 Then
                    PictureBox18.Image = Nothing
                Else
                    PictureBox18.Visible = True
                End If
            Case 23
                If card21() = 1 Then
                    PictureBox18.Image = Nothing
                Else
                    PictureBox18.Visible = True
                End If
            Case 24
                If card22() = 1 Then
                    PictureBox18.Image = Nothing
                Else
                    PictureBox18.Visible = True
                End If
            Case 25
                If card23() = 1 Then
                    PictureBox18.Image = Nothing
                Else
                    PictureBox18.Visible = True
                End If
        End Select
    End Sub

    Private Sub PictureBox19_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox19.Click
        Select Case carduri_functie(10)
            Case 0
                If card0() = 1 Then
                    PictureBox19.Image = Nothing
                Else
                    PictureBox19.Visible = True
                End If
            Case 1
                If card1() = 1 Then
                    PictureBox19.Image = Nothing
                Else
                    PictureBox19.Visible = True
                End If
            Case 2
                If card2() = 1 Then
                    PictureBox19.Image = Nothing
                Else
                    PictureBox19.Visible = True
                End If
            Case 3
                If card3() = 1 Then
                    PictureBox19.Image = Nothing
                Else
                    PictureBox19.Visible = True
                End If
            Case 4
                If card4() = 1 Then
                    PictureBox19.Image = Nothing
                Else
                    PictureBox19.Visible = True
                End If
            Case 5
                If card5() = 1 Then
                    PictureBox19.Image = Nothing
                Else
                    PictureBox19.Visible = True
                End If
            Case 6
                If card6() = 1 Then
                    PictureBox19.Image = Nothing
                Else
                    PictureBox19.Visible = True
                End If
            Case 7
                If card7() = 1 Then
                    PictureBox19.Image = Nothing
                Else
                    PictureBox19.Visible = True
                End If
            Case 8
                If card8() = 1 Then
                    PictureBox19.Image = Nothing
                Else
                    PictureBox19.Visible = True
                End If
            Case 9
                If card9() = 1 Then
                    PictureBox19.Image = Nothing
                Else
                    PictureBox19.Visible = True
                End If
            Case 10
                If card10() = 1 Then
                    PictureBox19.Image = Nothing
                Else
                    PictureBox19.Visible = True
                End If
            Case 11
                If card11() = 1 Then
                    PictureBox19.Image = Nothing
                Else
                    PictureBox19.Visible = True
                End If
            Case 12
                If card12() = 1 Then
                    PictureBox19.Image = Nothing
                Else
                    PictureBox19.Visible = True
                End If
            Case 13
                If card13() = 1 Then
                    PictureBox19.Image = Nothing
                Else
                    PictureBox19.Visible = True
                End If
            Case 14
                If card14() = 1 Then
                    PictureBox19.Image = Nothing
                Else
                    PictureBox19.Visible = True
                End If
            Case 15
                If card_daze() = 1 Then
                    PictureBox19.Image = Nothing
                Else
                    PictureBox19.Visible = True
                End If
            Case 16
                If card_wound() = 1 Then
                    PictureBox19.Image = Nothing
                Else
                    PictureBox19.Visible = True
                End If
            Case 17
                If card15() = 1 Then
                    PictureBox19.Image = Nothing
                Else
                    PictureBox19.Visible = True
                End If
            Case 18
                If card16() = 1 Then
                    PictureBox19.Image = Nothing
                Else
                    PictureBox19.Visible = True
                End If
            Case 19
                If card17() = 1 Then
                    PictureBox19.Image = Nothing
                Else
                    PictureBox19.Visible = True
                End If
            Case 20
                If card18() = 1 Then
                    PictureBox19.Image = Nothing
                Else
                    PictureBox19.Visible = True
                End If
            Case 21
                If card19() = 1 Then
                    PictureBox19.Image = Nothing
                Else
                    PictureBox19.Visible = True
                End If
            Case 22
                If card20() = 1 Then
                    PictureBox19.Image = Nothing
                Else
                    PictureBox19.Visible = True
                End If
            Case 23
                If card21() = 1 Then
                    PictureBox19.Image = Nothing
                Else
                    PictureBox19.Visible = True
                End If
            Case 24
                If card22() = 1 Then
                    PictureBox19.Image = Nothing
                Else
                    PictureBox19.Visible = True
                End If
            Case 25
                If card23() = 1 Then
                    PictureBox19.Image = Nothing
                Else
                    PictureBox19.Visible = True
                End If
        End Select
    End Sub
End Class
