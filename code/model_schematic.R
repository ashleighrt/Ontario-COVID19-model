

grViz("
      digraph model_schematic {
      graph [overlap = true, fontsize =12]
      
      #node statements
      node [fontname = Helvetica, 
            shape = rectangle, 
            width=2,
            height=0.75
            penwidth=2,
            fixedsize=TRUE]
            
      Susceptible [color=SeaGreen]
      Isolated [color = OrangeRed]
      Dead [color = Black]
      ICU [color = SlateBlue4]
      Exposed [color = Gold]
      Hospitalized [color = SlateBlue2]
      E_A [label ='@@3', color = Maroon]
      I_MILD [label ='@@4', color=Maroon]
      I_SEVERE [label ='@@5' , color = Maroon]
      IPQ [label ='@@6', color=OrangeRed]
      H_S [label ='@@7', color=SlateBlue2]
      Recovered [color=LightSlateGray]
      ExposedQ[label ='@@8', color=Gold]
      IQ [label ='@@1', color=OrangeRed]
      H_ICU [label ='@@2', color=SlateBlue2]
      IMQ [label ='@@9', color=OrangeRed]
     

      
      # edge statements
      Susceptible-> {Exposed ExposedQ}
      Exposed-> {E_A}
      ExposedQ -> IPQ
      IPQ -> {IQ IMQ} 
      IQ -> {Hospitalized H_S}
      IMQ -> Recovered
      E_A -> {I_MILD I_SEVERE}
      I_MILD -> {Recovered Isolated}
      I_SEVERE -> {Hospitalized H_S}
      Isolated -> Recovered
      Hospitalized -> Recovered
      H_S->ICU
      ICU->{H_ICU Dead}
      H_ICU->Recovered
      
      subgraph {
rank = same; Hospitalized; H_S

      }

      subgraph {
rank = same; Recovered; Dead

      }

  subgraph {
rank = same; Exposed; ExposedQ

  }
      
      subgraph {
rank = same; I_MILD; I_SEVERE; IQ; IMQ


        }

      
  }    
      [1]: paste0('Infectious\\n(severe, isolated)')
      [2]: paste0('Hospitalized\\n(post-ICU)')
      [3]: paste0('Infectious\\n(pre-symptomatic)')
      [4]: paste0('Infectious\\n(mild)')
      [5]: paste0('Infectious\\n(severe)')
      [6]: paste0('Infectious\\n(pre-symptomatic,\\nisolated)')
      [7]: paste0('Hospitalized\\n(pre-ICU)')
      [8]: paste0('Exposed\\n(quarantined)')
      [9]: paste0('Infectious\\n(mild, isolated)')

      ") -> model.schematic


  #DiagrammeRsvg::export_svg(model.schematic) %>% charToRaw %>% rsvg::rsvg_png("outputs/model_schematic.png")
